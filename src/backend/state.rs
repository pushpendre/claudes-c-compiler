//! Shared codegen state, slot addressing types, and register value cache.
//!
//! All three backends use the same `CodegenState` to track stack slot assignments,
//! alloca metadata, label generation, and the register value cache during code generation.
//! The `SlotAddr` enum captures the 3-way addressing pattern (over-aligned alloca /
//! direct alloca / indirect) that repeats across store, load, GEP, and memcpy emission.
//!
//! The `RegCache` tracks which IR values are currently known to be in registers,
//! enabling backends to skip redundant stack loads. This is the foundation for
//! eventually replacing the pure stack-slot model with a register allocator.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::common::types::IrType;
use super::common::AsmOutput;

/// Stack slot location for a value. Interpretation varies by arch:
/// - x86: negative offset from %rbp
/// - ARM: positive offset from sp
/// - RISC-V: negative offset from s0
#[derive(Debug, Clone, Copy)]
pub struct StackSlot(pub i64);

/// Register cache entry: tracks which IR value is known to be in a register.
/// The `is_alloca` flag distinguishes whether the register holds the alloca's
/// address (leaq/adr) or the value loaded from the stack slot (movq/ldr).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegCacheEntry {
    pub value_id: u32,
    pub is_alloca: bool,
}

/// Register value cache. Tracks which IR values are currently in the accumulator
/// and secondary registers, avoiding redundant stack loads.
///
/// The cache is conservative: it is invalidated on any operation that might clobber
/// a register (calls, inline asm, complex operations that use scratch registers).
/// This is safe because a stale entry would just cause a redundant load (the same
/// behavior as before the cache existed), while a missing invalidation could cause
/// incorrect code by skipping a needed load.
///
/// Architecture mapping:
/// - x86:    acc = %rax,  sec = %rcx
/// - ARM64:  acc = x0,    sec = x1
/// - RISC-V: acc = t0,    sec = t1
#[derive(Debug, Default)]
pub struct RegCache {
    /// Which value is currently in the primary accumulator register.
    pub acc: Option<RegCacheEntry>,
}

impl RegCache {
    /// Record that the accumulator now holds the given value.
    #[inline]
    pub fn set_acc(&mut self, value_id: u32, is_alloca: bool) {
        self.acc = Some(RegCacheEntry { value_id, is_alloca });
    }

    /// Check if the accumulator holds the given value (with matching alloca status).
    #[inline]
    pub fn acc_has(&self, value_id: u32, is_alloca: bool) -> bool {
        self.acc == Some(RegCacheEntry { value_id, is_alloca })
    }

    /// Invalidate the accumulator cache.
    #[inline]
    pub fn invalidate_acc(&mut self) {
        self.acc = None;
    }

    /// Invalidate all cached register values. Called on operations that may
    /// clobber any register (calls, inline asm, etc.).
    #[inline]
    pub fn invalidate_all(&mut self) {
        self.acc = None;
    }
}

/// Shared codegen state, used by all backends.
pub struct CodegenState {
    pub out: AsmOutput,
    pub stack_offset: i64,
    pub value_locations: FxHashMap<u32, StackSlot>,
    /// Values that are allocas (their stack slot IS the data, not a pointer to data).
    pub alloca_values: FxHashSet<u32>,
    /// Type associated with each alloca (for type-aware loads/stores).
    pub alloca_types: FxHashMap<u32, IrType>,
    /// Alloca values that need runtime alignment > 16 bytes.
    pub alloca_alignments: FxHashMap<u32, usize>,
    /// Values that are 128-bit integers (need 16-byte copy).
    pub i128_values: FxHashSet<u32>,
    /// Counter for generating unique labels (e.g., memcpy loops).
    label_counter: u32,
    /// Whether position-independent code (PIC) generation is enabled.
    pub pic_mode: bool,
    /// Set of symbol names that are locally defined (not extern) and have internal
    /// linkage (static) — these can use direct addressing even in PIC mode.
    pub local_symbols: FxHashSet<String>,
    /// Whether the current function contains DynAlloca instructions.
    /// When true, the epilogue must restore SP from the frame pointer instead of
    /// adding back the compile-time frame size.
    pub has_dyn_alloca: bool,
    /// Register value cache: tracks which IR values are in the accumulator and
    /// secondary registers to skip redundant loads.
    pub reg_cache: RegCache,
    /// Whether to replace `ret` with `jmp __x86_return_thunk` (-mfunction-return=thunk-extern).
    /// Used by the Linux kernel for Spectre v2 (retbleed) mitigation.
    pub function_return_thunk: bool,
    /// Whether to replace indirect calls/jumps with retpoline thunks (-mindirect-branch=thunk-extern).
    /// Used by the Linux kernel for Spectre v2 (retpoline) mitigation.
    pub indirect_branch_thunk: bool,
    /// Patchable function entry: (total_nops, nops_before_entry).
    /// When set, emits NOP padding around function entry points and records
    /// them in __patchable_function_entries for runtime patching (ftrace).
    pub patchable_function_entry: Option<(u32, u32)>,
    /// Whether to emit endbr64 at function entry points (-fcf-protection=branch).
    pub cf_protection_branch: bool,
    /// For x86 F128 (long double) precision: maps a value ID (result of an F128 load)
    /// to the pointer value ID it was loaded from. This allows emit_cast to use `fldt`
    /// directly from the original memory location instead of going through the f64
    /// intermediate in %rax, preserving full 80-bit precision for casts like
    /// `(unsigned long long)long_double_var`.
    pub f128_load_sources: FxHashMap<u32, u32>,
    /// Values whose 16-byte slots contain full x87 80-bit data (via fstpt),
    /// not a pointer. These are F128 call results or other values where
    /// full precision was preserved directly in the slot.
    pub f128_direct_slots: FxHashSet<u32>,
}

impl CodegenState {
    pub fn new() -> Self {
        Self {
            out: AsmOutput::new(),
            stack_offset: 0,
            value_locations: FxHashMap::default(),
            alloca_values: FxHashSet::default(),
            alloca_types: FxHashMap::default(),
            alloca_alignments: FxHashMap::default(),
            i128_values: FxHashSet::default(),
            label_counter: 0,
            pic_mode: false,
            local_symbols: FxHashSet::default(),
            has_dyn_alloca: false,
            reg_cache: RegCache::default(),
            function_return_thunk: false,
            indirect_branch_thunk: false,
            patchable_function_entry: None,
            cf_protection_branch: false,
            f128_load_sources: FxHashMap::default(),
            f128_direct_slots: FxHashSet::default(),
        }
    }

    pub fn next_label_id(&mut self) -> u32 {
        let id = self.label_counter;
        self.label_counter += 1;
        id
    }

    /// Generate a fresh label with the given prefix.
    pub fn fresh_label(&mut self, prefix: &str) -> String {
        let id = self.next_label_id();
        format!(".L{}_{}", prefix, id)
    }

    pub fn emit(&mut self, s: &str) {
        self.out.emit(s);
    }

    /// Emit formatted assembly directly (no temporary String allocation).
    #[inline]
    pub fn emit_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        self.out.emit_fmt(args);
    }

    pub fn reset_for_function(&mut self) {
        self.stack_offset = 0;
        self.value_locations.clear();
        self.alloca_values.clear();
        self.alloca_types.clear();
        self.alloca_alignments.clear();
        self.i128_values.clear();
        self.has_dyn_alloca = false;
        self.reg_cache.invalidate_all();
        self.f128_direct_slots.clear();
        self.f128_load_sources.clear();
    }

    /// Get the over-alignment requirement for an alloca (> 16 bytes), or None.
    pub fn alloca_over_align(&self, v: u32) -> Option<usize> {
        self.alloca_alignments.get(&v).copied()
    }

    pub fn is_alloca(&self, v: u32) -> bool {
        self.alloca_values.contains(&v)
    }

    pub fn get_slot(&self, v: u32) -> Option<StackSlot> {
        self.value_locations.get(&v).copied()
    }

    pub fn is_i128_value(&self, v: u32) -> bool {
        self.i128_values.contains(&v)
    }

    /// Returns true if the given symbol needs GOT indirection in PIC mode.
    /// A symbol needs GOT if PIC is enabled AND it's not a local (static) symbol.
    /// Local labels (starting with '.') are always PIC-safe via RIP-relative.
    pub fn needs_got(&self, name: &str) -> bool {
        if !self.pic_mode {
            return false;
        }
        if name.starts_with('.') {
            return false;
        }
        !self.local_symbols.contains(name)
    }

    /// Returns true if a function call needs PLT indirection in PIC mode.
    pub fn needs_plt(&self, name: &str) -> bool {
        self.needs_got(name)
    }
}

/// How a value's effective address is accessed. This captures the 3-way decision
/// (alloca with over-alignment / alloca direct / non-alloca indirect) that repeats
/// across emit_store, emit_load, emit_gep, and emit_memcpy.
#[derive(Debug, Clone, Copy)]
pub enum SlotAddr {
    /// Alloca with alignment > 16: runtime-aligned address must be computed.
    OverAligned(StackSlot, u32),
    /// Normal alloca: slot IS the data, access directly.
    Direct(StackSlot),
    /// Non-alloca: slot holds a pointer that must be loaded first.
    Indirect(StackSlot),
}

impl CodegenState {
    /// Classify how to access a value's effective address.
    /// Returns `None` if the value has no assigned stack slot.
    pub fn resolve_slot_addr(&self, val_id: u32) -> Option<SlotAddr> {
        let slot = self.get_slot(val_id)?;
        if self.is_alloca(val_id) {
            if self.alloca_over_align(val_id).is_some() {
                Some(SlotAddr::OverAligned(slot, val_id))
            } else {
                Some(SlotAddr::Direct(slot))
            }
        } else {
            Some(SlotAddr::Indirect(slot))
        }
    }
}
