use std::sync::Arc;

/// Represents C types in the compiler.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    Struct(Arc<StructType>),
    Union(Arc<StructType>),
    Enum(EnumType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub return_type: CType,
    pub params: Vec<(CType, Option<String>)>,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub name: Option<String>,
    pub fields: Vec<StructField>,
    /// If true, the struct is __attribute__((packed)) — alignment 1 for all fields.
    pub is_packed: bool,
    /// Maximum field alignment imposed by #pragma pack(N) or __attribute__((packed)).
    /// None means use natural alignment. Some(1) for packed, Some(N) for #pragma pack(N).
    pub max_field_align: Option<usize>,
    /// Cached size in bytes (computed once at construction).
    cached_size: usize,
    /// Cached alignment in bytes (computed once at construction).
    cached_align: usize,
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.fields == other.fields
            && self.is_packed == other.is_packed
            && self.max_field_align == other.max_field_align
    }
}

impl Eq for StructType {}

impl StructType {
    /// Create a new StructType for a struct, eagerly computing and caching size/align.
    pub fn new_struct(name: Option<String>, fields: Vec<StructField>, is_packed: bool, max_field_align: Option<usize>) -> Self {
        let layout = StructLayout::for_struct_with_packing(&fields, max_field_align);
        StructType {
            name,
            fields,
            is_packed,
            max_field_align,
            cached_size: layout.size,
            cached_align: layout.align,
        }
    }

    /// Create a new StructType for a union, eagerly computing and caching size/align.
    pub fn new_union(name: Option<String>, fields: Vec<StructField>, is_packed: bool, max_field_align: Option<usize>) -> Self {
        let layout = StructLayout::for_union(&fields);
        StructType {
            name,
            fields,
            is_packed,
            max_field_align,
            cached_size: layout.size,
            cached_align: layout.align,
        }
    }

    /// Create a forward-declared or empty struct/union type (no fields yet).
    pub fn new_empty(name: Option<String>, is_packed: bool, max_field_align: Option<usize>) -> Self {
        StructType {
            name,
            fields: Vec::new(),
            is_packed,
            max_field_align,
            cached_size: 0,
            cached_align: 1,
        }
    }

    /// Get the cached size in bytes.
    pub fn size(&self) -> usize {
        self.cached_size
    }

    /// Get the cached alignment in bytes.
    pub fn align(&self) -> usize {
        self.cached_align
    }

    /// Look up a field by name, returning its byte offset and type reference.
    /// For unions, all fields have offset 0.
    /// For structs, computes offset by iterating fields (uses cached size/align).
    /// Also searches anonymous struct/union members recursively.
    pub fn find_field(&self, name: &str, is_union: bool) -> Option<(usize, &CType)> {
        if is_union {
            // Union: all fields at offset 0
            for field in &self.fields {
                if field.name == name {
                    return Some((0, &field.ty));
                }
                // Search anonymous members
                if field.name.is_empty() {
                    if let Some((inner_offset, ty)) = Self::find_field_in_anon(&field.ty, name) {
                        return Some((inner_offset, ty));
                    }
                }
            }
            None
        } else {
            // Struct: compute offset by iterating
            let mut offset = 0usize;
            let max_field_align = self.max_field_align;
            for field in &self.fields {
                if field.bit_width.is_some() {
                    // Skip bitfield offset computation for now - fall through to full layout
                    // if the target field is in a bitfield region
                    continue;
                }
                let natural_align = field.ty.align();
                let field_align = if let Some(max_a) = max_field_align {
                    natural_align.min(max_a)
                } else {
                    natural_align
                };
                offset = align_up(offset, field_align);
                if field.name == name {
                    return Some((offset, &field.ty));
                }
                // Search anonymous members
                if field.name.is_empty() {
                    if let Some((inner_offset, ty)) = Self::find_field_in_anon(&field.ty, name) {
                        return Some((offset + inner_offset, ty));
                    }
                }
                offset += field.ty.size();
            }
            None
        }
    }

    /// Search for a field in an anonymous struct/union member.
    fn find_field_in_anon<'a>(ty: &'a CType, name: &str) -> Option<(usize, &'a CType)> {
        match ty {
            CType::Struct(st) => st.find_field(name, false),
            CType::Union(st) => st.find_field(name, true),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: CType,
    pub bit_width: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Compute the layout for a struct (fields laid out sequentially with alignment padding).
    /// Supports bitfield packing: adjacent bitfields share storage units.
    /// `max_field_align`: if Some(N), cap each field's alignment to min(natural, N).
    ///   For __attribute__((packed)), pass Some(1).
    ///   For #pragma pack(N), pass Some(N).
    ///   For normal structs, pass None.
    pub fn for_struct_with_packing(fields: &[StructField], max_field_align: Option<usize>) -> Self {
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
            let natural_align = field.ty.align();
            // Apply packing: cap alignment to max_field_align if specified
            let field_align = if let Some(max_a) = max_field_align {
                natural_align.min(max_a)
            } else {
                natural_align
            };
            let field_size = field.ty.size();
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
                    // For packed bitfields, we need to use the field's declared type size
                    // as the storage unit, placed at the byte boundary
                    let storage_offset = byte_offset;
                    let bit_offset_in_storage = bit_in_byte;

                    field_layouts.push(StructFieldLayout {
                        name: field.name.clone(),
                        offset: storage_offset,
                        ty: field.ty.clone(),
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
                        offset = align_up(offset, field_align);
                        bf_unit_offset = offset;
                        bf_unit_size = field_size;
                        bf_bit_pos = 0;
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
        }
    }

    /// Convenience wrapper: compute layout with default (no packing).
    pub fn for_struct(fields: &[StructField]) -> Self {
        Self::for_struct_with_packing(fields, None)
    }

    /// Compute the layout for a union (all fields at offset 0, size = max field size).
    pub fn for_union(fields: &[StructField]) -> Self {
        let mut max_size = 0usize;
        let mut max_align = 1usize;
        let mut field_layouts = Vec::with_capacity(fields.len());

        for field in fields {
            let field_align = field.ty.align();
            let field_size = field.ty.size();
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
    pub fn resolve_init_field_idx(&self, designator_name: Option<&str>, current_idx: usize) -> Option<usize> {
        if let Some(name) = designator_name {
            self.fields.iter().position(|f| f.name == name)
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
                    return Some(idx);
                }
            }
            None
        }
    }

    /// Look up a field by name, returning its offset and type.
    /// Recursively searches anonymous struct/union members.
    pub fn field_offset(&self, name: &str) -> Option<(usize, &CType)> {
        // First, try direct field lookup
        if let Some(f) = self.fields.iter().find(|f| f.name == name) {
            return Some((f.offset, &f.ty));
        }
        // Then, search anonymous (unnamed) struct/union members recursively
        for f in &self.fields {
            if !f.name.is_empty() {
                continue;
            }
            let anon_fields = match &f.ty {
                CType::Struct(st) | CType::Union(st) => &st.fields,
                _ => continue,
            };
            // Check if the target field is directly in this anonymous member
            if let Some(inner_field) = anon_fields.iter().find(|sf| sf.name == name) {
                // Compute offset within the anonymous struct/union
                let inner_offset = match &f.ty {
                    CType::Struct(_) => {
                        let layout = StructLayout::for_struct(anon_fields);
                        layout.field_offset(name).map(|(o, _)| o).unwrap_or(0)
                    }
                    CType::Union(_) => 0, // all union fields at offset 0
                    _ => 0,
                };
                return Some((f.offset + inner_offset, &inner_field.ty));
            }
            // Recurse into nested anonymous members
            let inner_layout = match &f.ty {
                CType::Struct(_) => StructLayout::for_struct(anon_fields),
                CType::Union(_) => StructLayout::for_union(anon_fields),
                _ => continue,
            };
            if let Some((inner_offset, _)) = inner_layout.field_offset(name) {
                // Found in a deeper nested anonymous - but we can't return
                // the &CType from the temporary layout. Search the anon fields
                // for a nested anonymous that contains the field.
                for sf in anon_fields {
                    if sf.name.is_empty() {
                        if let Some(deep_ty) = Self::find_field_type_in_anon(&sf.ty, name) {
                            return Some((f.offset + inner_offset, deep_ty));
                        }
                    }
                }
            }
        }
        None
    }

    /// Helper: find a field's type reference within an anonymous struct/union type.
    fn find_field_type_in_anon<'a>(ty: &'a CType, name: &str) -> Option<&'a CType> {
        let fields = match ty {
            CType::Struct(st) | CType::Union(st) => &st.fields,
            _ => return None,
        };
        for f in fields {
            if f.name == name {
                return Some(&f.ty);
            }
            if f.name.is_empty() {
                if let Some(deep) = Self::find_field_type_in_anon(&f.ty, name) {
                    return Some(deep);
                }
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
    (offset + align - 1) & !(align - 1)
}

impl CType {
    /// Size in bytes on a 64-bit target.
    pub fn size(&self) -> usize {
        match self {
            CType::Void => 0,
            CType::Bool | CType::Char | CType::UChar => 1,
            CType::Short | CType::UShort => 2,
            CType::Int | CType::UInt => 4,
            CType::Long | CType::ULong => 8,
            CType::LongLong | CType::ULongLong => 8,
            CType::Float => 4,
            CType::Double => 8,
            CType::LongDouble => 16,
            CType::ComplexFloat => 8,     // 2 * sizeof(float)
            CType::ComplexDouble => 16,   // 2 * sizeof(double)
            CType::ComplexLongDouble => 32, // 2 * sizeof(long double) = 2 * 16
            CType::Pointer(_) => 8,
            CType::Array(elem, Some(n)) => elem.size() * n,
            CType::Array(_, None) => 8, // incomplete array treated as pointer
            CType::Function(_) => 8, // function pointer size
            CType::Struct(s) => s.size(),
            CType::Union(s) => s.size(),
            CType::Enum(_) => 4,
        }
    }

    /// Alignment in bytes on a 64-bit target.
    pub fn align(&self) -> usize {
        match self {
            CType::Void => 1,
            CType::Bool | CType::Char | CType::UChar => 1,
            CType::Short | CType::UShort => 2,
            CType::Int | CType::UInt => 4,
            CType::Long | CType::ULong => 8,
            CType::LongLong | CType::ULongLong => 8,
            CType::Float => 4,
            CType::Double => 8,
            CType::LongDouble => 16,
            CType::ComplexFloat => 4,       // align of float component
            CType::ComplexDouble => 8,      // align of double component
            CType::ComplexLongDouble => 16, // align of long double (F128) component
            CType::Pointer(_) => 8,
            CType::Array(elem, _) => elem.align(),
            CType::Function(_) => 8,
            CType::Struct(s) => s.align(),
            CType::Union(s) => s.align(),
            CType::Enum(_) => 4,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, CType::Bool | CType::Char | CType::UChar | CType::Short | CType::UShort |
                       CType::Int | CType::UInt | CType::Long | CType::ULong |
                       CType::LongLong | CType::ULongLong | CType::Enum(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, CType::Char | CType::Short | CType::Int | CType::Long | CType::LongLong)
    }

    /// Whether this is a complex type (_Complex float/double/long double).
    pub fn is_complex(&self) -> bool {
        matches!(self, CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble)
    }

    /// Whether this is a floating-point type (float, double, long double).
    pub fn is_floating(&self) -> bool {
        matches!(self, CType::Float | CType::Double | CType::LongDouble)
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
    U8,
    U16,
    U32,
    U64,
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
        matches!(self, IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64)
    }

    /// Whether this is a signed integer type.
    pub fn is_signed(&self) -> bool {
        matches!(self, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64)
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

    /// Get the unsigned counterpart of this type.
    pub fn to_unsigned(&self) -> Self {
        match self {
            IrType::I8 => IrType::U8,
            IrType::I16 => IrType::U16,
            IrType::I32 => IrType::U32,
            IrType::I64 => IrType::U64,
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
            CType::Enum(_) => IrType::U32,
            CType::UInt => IrType::U32,
            CType::Long | CType::LongLong => IrType::I64,
            CType::ULong | CType::ULongLong => IrType::U64,
            CType::Float => IrType::F32,
            CType::Double => IrType::F64,
            CType::LongDouble => IrType::F128,
            // Complex types are handled as aggregate (pointer to stack slot)
            CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble => IrType::Ptr,
            CType::Pointer(_) | CType::Array(_, _) | CType::Function(_) => IrType::Ptr,
            CType::Struct(_) | CType::Union(_) => IrType::Ptr, // TODO: handle aggregates properly
        }
    }
}
