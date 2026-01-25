use crate::common::fx_hash::FxHashMap;

/// Trait for looking up struct/union layout information.
/// TypeContext implements this trait, allowing CType methods in common/
/// to resolve struct/union sizes and alignments without depending on
/// the lowering module directly.
pub trait StructLayoutProvider {
    fn get_struct_layout(&self, key: &str) -> Option<&StructLayout>;
}

/// A HashMap-based provider for struct layouts (used by TypeContext and sema).
impl StructLayoutProvider for FxHashMap<String, StructLayout> {
    fn get_struct_layout(&self, key: &str) -> Option<&StructLayout> {
        self.get(key)
    }
}

/// Represents C types in the compiler.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CType {
    Void,
    Bool,
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Int128,
    UInt128,
    Float,
    Double,
    LongDouble,
    /// C99 _Complex float: two f32 values (real, imag)
    ComplexFloat,
    /// C99 _Complex double: two f64 values (real, imag)
    ComplexDouble,
    /// C99 _Complex long double: two f128 values (real, imag) - uses F128 storage per component
    ComplexLongDouble,
    Pointer(Box<CType>),
    Array(Box<CType>, Option<usize>),
    Function(Box<FunctionType>),
    /// Struct type, identified by key (e.g., "struct.Foo" or "__anon_struct_7").
    /// The actual layout is stored in TypeContext's struct_layouts map.
    Struct(String),
    /// Union type, identified by key (e.g., "union.Bar" or "__anon_struct_7").
    /// The actual layout is stored in TypeContext's struct_layouts map.
    Union(String),
    Enum(EnumType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub return_type: CType,
    pub params: Vec<(CType, Option<String>)>,
    pub variadic: bool,
}

/// StructField describes a single field in a struct or union declaration.
/// Used during construction of StructLayout.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: String,
    pub ty: CType,
    pub bit_width: Option<u32>,
    /// Per-field alignment override from _Alignas(N) or __attribute__((aligned(N))).
    /// When set, this overrides the natural alignment of the field's type.
    pub alignment: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub name: Option<String>,
    pub variants: Vec<(String, i64)>,
}

/// Computed layout for a struct or union, with field offsets and total size.
#[derive(Debug, Clone)]
pub struct StructLayout {
    /// Each field's (name, byte offset, CType).
    pub fields: Vec<StructFieldLayout>,
    /// Total size of the struct in bytes (including trailing padding).
    pub size: usize,
    /// Required alignment of the struct.
    pub align: usize,
    /// Whether this is a union (all fields at offset 0).
    pub is_union: bool,
    /// Whether this union has `__attribute__((transparent_union))`.
    /// A transparent union parameter is passed using the ABI of its first member.
    pub is_transparent_union: bool,
}

/// Result of resolving a designated initializer field name.
#[derive(Debug, Clone)]
pub enum InitFieldResolution {
    /// Found as a direct field at this index.
    Direct(usize),
    /// Found inside an anonymous struct/union member at the given index.
    /// The String is the original designator name to use when drilling into
    /// the anonymous member.
    AnonymousMember { anon_field_idx: usize, inner_name: String },
}

/// Layout info for a single field.
#[derive(Debug, Clone)]
pub struct StructFieldLayout {
    pub name: String,
    pub offset: usize,
    pub ty: CType,
    /// For bitfields: bit offset within the storage unit at `offset`.
    pub bit_offset: Option<u32>,
    /// For bitfields: width in bits.
    pub bit_width: Option<u32>,
}

impl StructLayout {
    /// Return the smallest integer CType that can hold at least `needed_bytes` bytes.
    /// Preserves signedness.
    fn smallest_int_ctype_for_bytes(needed_bytes: usize, is_signed: bool) -> CType {
        if is_signed {
            match needed_bytes {
                0..=1 => CType::Char,
                2 => CType::Short,
                3..=4 => CType::Int,
                _ => CType::Long,
            }
        } else {
            match needed_bytes {
                0..=1 => CType::UChar,
                2 => CType::UShort,
                3..=4 => CType::UInt,
                _ => CType::ULong,
            }
        }
    }

    /// Compute the layout for a struct (fields laid out sequentially with alignment padding).
    /// Supports bitfield packing: adjacent bitfields share storage units.
    /// `max_field_align`: if Some(N), cap each field's alignment to min(natural, N).
    ///   For __attribute__((packed)), pass Some(1).
    ///   For #pragma pack(N), pass Some(N).
    ///   For normal structs, pass None.
    /// `ctx`: provides struct/union layout lookup for nested struct/union field types.
    pub fn for_struct_with_packing(fields: &[StructField], max_field_align: Option<usize>, ctx: &dyn StructLayoutProvider) -> Self {
        let mut offset = 0usize;
        let mut max_align = 1usize;
        let mut field_layouts = Vec::with_capacity(fields.len());

        // Track current bitfield storage unit
        let mut bf_unit_offset = 0usize; // byte offset of current bitfield storage unit
        let mut bf_bit_pos = 0u32;       // current bit position within the storage unit
        let mut bf_unit_size = 0usize;   // size of the current storage unit in bytes
        let mut in_bitfield = false;
        // For packed structs with pack(1), track total bit position for contiguous bitfield packing
        let is_packed_1 = max_field_align == Some(1);

        for field in fields {
            let natural_align = field.ty.align_ctx(ctx);
            // Per-field alignment override: _Alignas(N) or __attribute__((aligned(N)))
            // sets a minimum alignment (can only increase, per C11 6.7.5).
            let overridden_align = if let Some(explicit) = field.alignment {
                natural_align.max(explicit)
            } else {
                natural_align
            };
            // Apply packing: cap alignment to max_field_align if specified.
            // Note: _Alignas / __attribute__((aligned)) on a field overrides packing
            // per GCC behavior — an explicit alignment attribute takes precedence.
            let field_align = if field.alignment.is_some() {
                // Explicit alignment attribute overrides packing
                overridden_align
            } else if let Some(max_a) = max_field_align {
                natural_align.min(max_a)
            } else {
                natural_align
            };
            let field_size = field.ty.size_ctx(ctx);
            max_align = max_align.max(field_align);

            if let Some(bw) = field.bit_width {
                if bw == 0 {
                    // Zero-width bitfield: force alignment to next storage unit boundary
                    if in_bitfield {
                        if is_packed_1 {
                            // For packed(1), advance to next byte boundary
                            let total_bits = (bf_unit_offset * 8) as u32 + bf_bit_pos;
                            offset = align_up((total_bits as usize + 7) / 8, 1);
                        } else {
                            offset = bf_unit_offset + bf_unit_size;
                        }
                    }
                    offset = align_up(offset, field_align);
                    in_bitfield = false;
                    bf_bit_pos = 0;
                    continue;
                }

                let unit_bits = (field_size * 8) as u32;

                if is_packed_1 {
                    // For pack(1), allow bitfields to span across storage unit boundaries.
                    // We track a contiguous bit stream.
                    if !in_bitfield {
                        // Start new bitfield sequence at current offset
                        bf_unit_offset = offset;
                        bf_bit_pos = 0;
                        bf_unit_size = field_size;
                        in_bitfield = true;
                    }
                    // The field's storage unit starts at the byte containing the current bit position
                    let total_bit_offset = (bf_unit_offset * 8) as u32 + bf_bit_pos;
                    let byte_offset = (total_bit_offset / 8) as usize;
                    let bit_in_byte = total_bit_offset % 8;
                    let storage_offset = byte_offset;
                    let bit_offset_in_storage = bit_in_byte;

                    // If the bitfield spans beyond its declared type's storage unit,
                    // widen the storage type to cover all needed bits.
                    // E.g., unsigned char m1:8 at bit_offset=4 needs 12 bits > 8,
                    // so we widen to unsigned short (16 bits).
                    let needed_bits = bit_offset_in_storage + bw;
                    let storage_ty = if needed_bits > unit_bits {
                        let needed_bytes = ((needed_bits + 7) / 8) as usize;
                        let is_signed = field.ty.is_signed();
                        Self::smallest_int_ctype_for_bytes(needed_bytes, is_signed)
                    } else {
                        field.ty.clone()
                    };

                    field_layouts.push(StructFieldLayout {
                        name: field.name.clone(),
                        offset: storage_offset,
                        ty: storage_ty,
                        bit_offset: Some(bit_offset_in_storage),
                        bit_width: Some(bw),
                    });

                    bf_bit_pos += bw;
                } else {
                    // Standard (non-packed) bitfield layout
                    if !in_bitfield || bf_bit_pos + bw > unit_bits || bf_unit_size != field_size {
                        // Start a new storage unit
                        if in_bitfield {
                            offset = bf_unit_offset + bf_unit_size;
                        }
                        // GCC-compatible bitfield placement: try to fit the bitfield
                        // within the naturally-aligned storage unit that contains the
                        // current offset. Only advance to a new aligned unit if the
                        // bitfield bits would overflow the current aligned unit.
                        let aligned_start = offset & !(field_align - 1); // align_down
                        let bit_pos_in_unit = ((offset - aligned_start) * 8) as u32;
                        if bit_pos_in_unit + bw <= unit_bits {
                            // Bitfield fits in the current aligned storage unit
                            bf_unit_offset = aligned_start;
                            bf_bit_pos = bit_pos_in_unit;
                        } else {
                            // Doesn't fit; start a new aligned storage unit
                            bf_unit_offset = align_up(offset, field_align);
                            bf_bit_pos = 0;
                        }
                        bf_unit_size = field_size;
                        in_bitfield = true;
                    }

                    field_layouts.push(StructFieldLayout {
                        name: field.name.clone(),
                        offset: bf_unit_offset,
                        ty: field.ty.clone(),
                        bit_offset: Some(bf_bit_pos),
                        bit_width: Some(bw),
                    });

                    bf_bit_pos += bw;
                }
            } else {
                // Regular (non-bitfield) field
                if in_bitfield {
                    if is_packed_1 {
                        // For pack(1), advance past all bits used
                        let total_bits = (bf_unit_offset * 8) as u32 + bf_bit_pos;
                        offset = (total_bits as usize + 7) / 8; // round up to next byte
                    } else {
                        offset = bf_unit_offset + bf_unit_size;
                    }
                    in_bitfield = false;
                }

                offset = align_up(offset, field_align);

                field_layouts.push(StructFieldLayout {
                    name: field.name.clone(),
                    offset,
                    ty: field.ty.clone(),
                    bit_offset: None,
                    bit_width: None,
                });

                // Flexible array member (e.g., int f[]) contributes 0 to struct size
                let is_flexible_array = matches!(&field.ty, CType::Array(_, None));
                if !is_flexible_array {
                    offset += field_size;
                }
            }
        }

        // Account for trailing bitfield
        if in_bitfield {
            if is_packed_1 {
                let total_bits = (bf_unit_offset * 8) as u32 + bf_bit_pos;
                offset = (total_bits as usize + 7) / 8;
            } else {
                offset = bf_unit_offset + bf_unit_size;
            }
        }

        // Pad total size to struct alignment
        let size = align_up(offset, max_align);

        StructLayout {
            fields: field_layouts,
            size,
            align: max_align,
            is_union: false,
            is_transparent_union: false,
        }
    }

    /// Convenience wrapper: compute layout with default (no packing).
    pub fn for_struct(fields: &[StructField], ctx: &dyn StructLayoutProvider) -> Self {
        Self::for_struct_with_packing(fields, None, ctx)
    }

    /// Compute the layout for a union (all fields at offset 0, size = max field size).
    pub fn for_union(fields: &[StructField], ctx: &dyn StructLayoutProvider) -> Self {
        let mut max_size = 0usize;
        let mut max_align = 1usize;
        let mut field_layouts = Vec::with_capacity(fields.len());

        for field in fields {
            let natural_align = field.ty.align_ctx(ctx);
            // Per-field alignment override from _Alignas(N) or __attribute__((aligned(N)))
            let field_align = if let Some(explicit) = field.alignment {
                natural_align.max(explicit)
            } else {
                natural_align
            };
            let field_size = field.ty.size_ctx(ctx);
            max_align = max_align.max(field_align);
            max_size = max_size.max(field_size);

            // For union bitfield members, set bit_offset to 0 (all fields start at byte 0)
            // and propagate bit_width so extraction is performed on read
            let (bf_offset, bf_width) = if let Some(bw) = field.bit_width {
                if bw > 0 {
                    (Some(0u32), Some(bw))
                } else {
                    (None, None) // zero-width bitfield
                }
            } else {
                (None, None)
            };

            field_layouts.push(StructFieldLayout {
                name: field.name.clone(),
                offset: 0, // All union fields start at offset 0
                ty: field.ty.clone(),
                bit_offset: bf_offset,
                bit_width: bf_width,
            });
        }

        let size = align_up(max_size, max_align);

        StructLayout {
            fields: field_layouts,
            size,
            align: max_align,
            is_union: true,
            is_transparent_union: false,
        }
    }

    /// Check if any field in this layout is or contains a pointer/function type.
    /// Recursively checks array element types and nested struct/union fields.
    pub fn has_pointer_fields(&self, ctx: &dyn StructLayoutProvider) -> bool {
        self.fields.iter().any(|f| Self::field_type_has_pointers(&f.ty, ctx))
    }

    /// Check if a type is or contains pointer/function types (recursive).
    fn field_type_has_pointers(ty: &CType, ctx: &dyn StructLayoutProvider) -> bool {
        match ty {
            CType::Pointer(_) | CType::Function(_) => true,
            CType::Array(inner, _) => Self::field_type_has_pointers(inner, ctx),
            CType::Struct(key) | CType::Union(key) => {
                if let Some(layout) = ctx.get_struct_layout(key) {
                    layout.fields.iter().any(|f| Self::field_type_has_pointers(&f.ty, ctx))
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Resolve which field index an initializer targets, given either a field designator
    /// or a positional index that skips unnamed bitfield fields.
    ///
    /// `designator_name`: If Some, look up the field by name.
    /// `current_idx`: The current positional index (used when no designator).
    ///
    /// Per C11 6.7.9p9, unnamed members of structure types do not participate in
    /// initialization. Anonymous struct/union members (empty name, no bit_width) DO
    /// participate. Unnamed bitfields (empty name, has bit_width) do NOT.
    ///
    /// Returns the resolved field index, or `None` if no valid field found.
    pub fn resolve_init_field_idx(&self, designator_name: Option<&str>, current_idx: usize, ctx: &dyn StructLayoutProvider) -> Option<usize> {
        match self.resolve_init_field(designator_name, current_idx, ctx) {
            Some(InitFieldResolution::Direct(idx)) => Some(idx),
            Some(InitFieldResolution::AnonymousMember { anon_field_idx, .. }) => Some(anon_field_idx),
            None => None,
        }
    }

    /// Resolve which field an initializer targets, with full info about anonymous members.
    ///
    /// When a designator name is found inside an anonymous struct/union member,
    /// returns `AnonymousMember` with the anonymous field's index and the inner name,
    /// allowing callers to drill into the anonymous member for proper initialization.
    pub fn resolve_init_field(&self, designator_name: Option<&str>, current_idx: usize, ctx: &dyn StructLayoutProvider) -> Option<InitFieldResolution> {
        if let Some(name) = designator_name {
            // First try direct field lookup
            if let Some(idx) = self.fields.iter().position(|f| f.name == name) {
                return Some(InitFieldResolution::Direct(idx));
            }
            // Search inside anonymous struct/union members
            for (idx, f) in self.fields.iter().enumerate() {
                // Anonymous members have empty name and no bit_width
                if !f.name.is_empty() || f.bit_width.is_some() {
                    continue;
                }
                match &f.ty {
                    CType::Struct(key) | CType::Union(key) => {
                        if Self::anon_member_contains_field_ctx(key, name, ctx) {
                            return Some(InitFieldResolution::AnonymousMember {
                                anon_field_idx: idx,
                                inner_name: name.to_string(),
                            });
                        }
                    }
                    _ => {}
                }
            }
            None
        } else {
            // Positional init: skip unnamed bitfields (empty name + has bit_width).
            // Anonymous struct/union members (empty name, no bit_width) still participate.
            let mut idx = current_idx;
            while idx < self.fields.len() {
                let f = &self.fields[idx];
                if f.name.is_empty() && f.bit_width.is_some() {
                    // Unnamed bitfield: skip it
                    idx += 1;
                } else {
                    return Some(InitFieldResolution::Direct(idx));
                }
            }
            None
        }
    }

    /// Check if an anonymous struct/union member (identified by layout key) contains
    /// a field with the given name, including recursively through nested anonymous members.
    fn anon_member_contains_field_ctx(key: &str, name: &str, ctx: &dyn StructLayoutProvider) -> bool {
        if let Some(layout) = ctx.get_struct_layout(key) {
            for f in &layout.fields {
                if f.name == name {
                    return true;
                }
                // Recurse into nested anonymous members
                if f.name.is_empty() {
                    match &f.ty {
                        CType::Struct(inner_key) | CType::Union(inner_key) => {
                            if Self::anon_member_contains_field_ctx(inner_key, name, ctx) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        false
    }

    /// Look up a field by name, returning its offset and a clone of its type.
    /// Recursively searches anonymous struct/union members.
    pub fn field_offset(&self, name: &str, ctx: &dyn StructLayoutProvider) -> Option<(usize, CType)> {
        // First, try direct field lookup
        if let Some(f) = self.fields.iter().find(|f| f.name == name) {
            return Some((f.offset, f.ty.clone()));
        }
        // Then, search anonymous (unnamed) struct/union members recursively
        for f in &self.fields {
            if !f.name.is_empty() {
                continue;
            }
            let anon_key = match &f.ty {
                CType::Struct(key) | CType::Union(key) => key.clone(),
                _ => continue,
            };
            let anon_layout = match ctx.get_struct_layout(&anon_key) {
                Some(layout) => layout.clone(),
                None => continue,
            };
            // Check if the target field is directly in this anonymous member
            if let Some(inner_field) = anon_layout.fields.iter().find(|sf| sf.name == name) {
                // Compute offset within the anonymous struct/union
                let inner_offset = match &f.ty {
                    CType::Struct(_) => {
                        anon_layout.field_offset(name, ctx).map(|(o, _)| o).unwrap_or(0)
                    }
                    CType::Union(_) => 0, // all union fields at offset 0
                    _ => 0,
                };
                return Some((f.offset + inner_offset, inner_field.ty.clone()));
            }
            // Recurse into nested anonymous members
            if let Some((inner_offset, ty)) = anon_layout.field_offset(name, ctx) {
                return Some((f.offset + inner_offset, ty));
            }
        }
        None
    }

    /// Look up a field by name, returning full layout info including bitfield details.
    pub fn field_layout(&self, name: &str) -> Option<&StructFieldLayout> {
        self.fields.iter().find(|f| f.name == name)
    }
}

/// Align `offset` up to the next multiple of `align`.
pub fn align_up(offset: usize, align: usize) -> usize {
    if align == 0 { return offset; }
    let mask = align - 1;
    match offset.checked_add(mask) {
        Some(v) => v & !mask,
        None => offset, // overflow: return offset unchanged
    }
}

impl CType {
    /// Size in bytes on a 64-bit target, with struct/union layout lookup via context.
    pub fn size_ctx(&self, ctx: &dyn StructLayoutProvider) -> usize {
        match self {
            CType::Void => 0,
            CType::Bool | CType::Char | CType::UChar => 1,
            CType::Short | CType::UShort => 2,
            CType::Int | CType::UInt => 4,
            CType::Long | CType::ULong => 8,
            CType::LongLong | CType::ULongLong => 8,
            CType::Int128 | CType::UInt128 => 16,
            CType::Float => 4,
            CType::Double => 8,
            CType::LongDouble => 16,
            CType::ComplexFloat => 8,     // 2 * sizeof(float)
            CType::ComplexDouble => 16,   // 2 * sizeof(double)
            CType::ComplexLongDouble => 32, // 2 * sizeof(long double) = 2 * 16
            CType::Pointer(_) => 8,
            CType::Array(elem, Some(n)) => elem.size_ctx(ctx) * n,
            CType::Array(_, None) => 8, // incomplete array treated as pointer
            CType::Function(_) => 8, // function pointer size
            CType::Struct(key) | CType::Union(key) => {
                ctx.get_struct_layout(key).map(|l| l.size).unwrap_or(0)
            }
            CType::Enum(_) => 4,
        }
    }

    /// Alignment in bytes on a 64-bit target, with struct/union layout lookup via context.
    pub fn align_ctx(&self, ctx: &dyn StructLayoutProvider) -> usize {
        match self {
            CType::Void => 1,
            CType::Bool | CType::Char | CType::UChar => 1,
            CType::Short | CType::UShort => 2,
            CType::Int | CType::UInt => 4,
            CType::Long | CType::ULong => 8,
            CType::LongLong | CType::ULongLong => 8,
            CType::Int128 | CType::UInt128 => 16,
            CType::Float => 4,
            CType::Double => 8,
            CType::LongDouble => 16,
            CType::ComplexFloat => 4,       // align of float component
            CType::ComplexDouble => 8,      // align of double component
            CType::ComplexLongDouble => 16, // align of long double (F128) component
            CType::Pointer(_) => 8,
            CType::Array(elem, _) => elem.align_ctx(ctx),
            CType::Function(_) => 8,
            CType::Struct(key) | CType::Union(key) => {
                ctx.get_struct_layout(key).map(|l| l.align).unwrap_or(1)
            }
            CType::Enum(_) => 4,
        }
    }

    /// Size in bytes for non-struct/union types. For struct/union, returns 0.
    /// Use size_ctx() when you need accurate struct/union sizes.
    pub fn size(&self) -> usize {
        match self {
            CType::Struct(_) | CType::Union(_) => 0,
            _ => {
                // For non-struct/union types, we can use an empty provider
                let empty: FxHashMap<String, StructLayout> = FxHashMap::default();
                self.size_ctx(&empty)
            }
        }
    }

    /// Alignment in bytes for non-struct/union types. For struct/union, returns 1.
    /// Use align_ctx() when you need accurate struct/union alignment.
    pub fn align(&self) -> usize {
        match self {
            CType::Struct(_) | CType::Union(_) => 1,
            _ => {
                let empty: FxHashMap<String, StructLayout> = FxHashMap::default();
                self.align_ctx(&empty)
            }
        }
    }

    /// Get the struct/union layout key if this is a CType::Struct or CType::Union.
    pub fn struct_key(&self) -> Option<&str> {
        match self {
            CType::Struct(key) | CType::Union(key) => Some(key),
            _ => None,
        }
    }

    /// Extract the tag name from a struct/union key (e.g., "struct.Foo" -> "Foo").
    pub fn struct_tag_name(&self) -> Option<&str> {
        match self {
            CType::Struct(key) | CType::Union(key) => {
                key.split('.').nth(1)
            }
            _ => None,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, CType::Bool | CType::Char | CType::UChar | CType::Short | CType::UShort |
                       CType::Int | CType::UInt | CType::Long | CType::ULong |
                       CType::LongLong | CType::ULongLong |
                       CType::Int128 | CType::UInt128 | CType::Enum(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, CType::Char | CType::Short | CType::Int | CType::Long | CType::LongLong | CType::Int128)
    }

    /// Whether this is a complex type (_Complex float/double/long double).
    pub fn is_complex(&self) -> bool {
        matches!(self, CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble)
    }

    /// Whether this is a floating-point type (float, double, long double).
    pub fn is_floating(&self) -> bool {
        matches!(self, CType::Float | CType::Double | CType::LongDouble)
    }

    /// Apply C integer promotion rules (C11 6.3.1.1):
    /// Types smaller than int are promoted to int (or unsigned int if int cannot
    /// represent all values, but for our targets int is 32-bit so Bool/Char/UChar/
    /// Short/UShort all fit in int).
    pub fn integer_promoted(&self) -> CType {
        match self {
            CType::Bool | CType::Char | CType::UChar
            | CType::Short | CType::UShort => CType::Int,
            other => other.clone(),
        }
    }

    /// Whether this is an arithmetic type (integer, floating, or complex).
    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_floating() || self.is_complex()
    }

    /// Get the component type for a complex type (e.g., ComplexFloat -> Float).
    pub fn complex_component_type(&self) -> CType {
        match self {
            CType::ComplexFloat => CType::Float,
            CType::ComplexDouble => CType::Double,
            CType::ComplexLongDouble => CType::LongDouble,
            _ => self.clone(), // not complex, return self
        }
    }

    /// Get the complex type for a given real component type.
    pub fn to_complex(&self) -> CType {
        match self {
            CType::Float => CType::ComplexFloat,
            CType::Double => CType::ComplexDouble,
            CType::LongDouble => CType::ComplexLongDouble,
            _ => CType::ComplexDouble, // default: promote to complex double
        }
    }

}

/// IR-level types (simpler than C types).
/// Signed and unsigned variants are tracked separately so that the backend
/// can choose sign-extension vs zero-extension appropriately.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrType {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    /// 128-bit floating point (long double on AArch64/RISC-V, 80-bit extended on x86-64).
    /// Computation is done in F64 precision; this type exists to ensure correct
    /// ABI handling (16-byte storage, proper variadic argument passing, va_arg).
    F128,
    Ptr,
    Void,
}

impl IrType {
    pub fn size(&self) -> usize {
        match self {
            IrType::I8 | IrType::U8 => 1,
            IrType::I16 | IrType::U16 => 2,
            IrType::I32 | IrType::U32 => 4,
            IrType::I64 | IrType::U64 | IrType::Ptr => 8,
            IrType::I128 | IrType::U128 => 16,
            IrType::F32 => 4,
            IrType::F64 => 8,
            IrType::F128 => 16,
            IrType::Void => 0,
        }
    }

    /// Alignment in bytes (matches size for all current IR types except Void which is 1).
    pub fn align(&self) -> usize {
        match self {
            IrType::Void => 1,
            other => other.size().max(1),
        }
    }

    /// Whether this is an unsigned integer type.
    pub fn is_unsigned(&self) -> bool {
        matches!(self, IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64 | IrType::U128)
    }

    /// Whether this is a signed integer type.
    pub fn is_signed(&self) -> bool {
        matches!(self, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 | IrType::I128)
    }

    /// Whether this is any integer type (signed or unsigned).
    pub fn is_integer(&self) -> bool {
        self.is_signed() || self.is_unsigned()
    }

    /// Whether this is a floating-point type (F32, F64, or F128).
    /// F128 (long double) is included because at computation level it is treated
    /// as F64 (stored in D registers), with 16-byte storage for ABI correctness.
    pub fn is_float(&self) -> bool {
        matches!(self, IrType::F32 | IrType::F64 | IrType::F128)
    }

    /// Whether this is a long double type (F128).
    /// Long double values are computed as F64 but stored as 16 bytes for ABI correctness.
    pub fn is_long_double(&self) -> bool {
        matches!(self, IrType::F128)
    }

    /// Whether this is a 128-bit integer type (I128 or U128).
    pub fn is_128bit(&self) -> bool {
        matches!(self, IrType::I128 | IrType::U128)
    }

    /// Get the unsigned counterpart of this type.
    pub fn to_unsigned(&self) -> Self {
        match self {
            IrType::I8 => IrType::U8,
            IrType::I16 => IrType::U16,
            IrType::I32 => IrType::U32,
            IrType::I64 => IrType::U64,
            IrType::I128 => IrType::U128,
            other => *other,
        }
    }

    /// Get the signed counterpart of this type.
    pub fn to_signed(&self) -> Self {
        match self {
            IrType::U8 => IrType::I8,
            IrType::U16 => IrType::I16,
            IrType::U32 => IrType::I32,
            IrType::U64 => IrType::I64,
            IrType::U128 => IrType::I128,
            other => *other,
        }
    }

    pub fn from_ctype(ct: &CType) -> Self {
        match ct {
            CType::Void => IrType::Void,
            CType::Bool => IrType::U8,
            CType::Char => IrType::I8,
            CType::UChar => IrType::U8,
            CType::Short => IrType::I16,
            CType::UShort => IrType::U16,
            CType::Int => IrType::I32,
            CType::Enum(_) => IrType::I32, // C enum underlying type is int (signed)
            CType::UInt => IrType::U32,
            CType::Long | CType::LongLong => IrType::I64,
            CType::ULong | CType::ULongLong => IrType::U64,
            CType::Int128 => IrType::I128,
            CType::UInt128 => IrType::U128,
            CType::Float => IrType::F32,
            CType::Double => IrType::F64,
            CType::LongDouble => IrType::F128,
            // Complex types are handled as aggregate (pointer to stack slot)
            CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble => IrType::Ptr,
            CType::Pointer(_) | CType::Array(_, _) | CType::Function(_) => IrType::Ptr,
            CType::Struct(_) | CType::Union(_) => IrType::Ptr,
        }
    }
}
