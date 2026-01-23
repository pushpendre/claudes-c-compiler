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
    Pointer(Box<CType>),
    Array(Box<CType>, Option<usize>),
    Function(Box<FunctionType>),
    Struct(StructType),
    Union(StructType),
    Enum(EnumType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub return_type: CType,
    pub params: Vec<(CType, Option<String>)>,
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: Option<String>,
    pub fields: Vec<StructField>,
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
    pub fn for_struct(fields: &[StructField]) -> Self {
        let mut offset = 0usize;
        let mut max_align = 1usize;
        let mut field_layouts = Vec::with_capacity(fields.len());

        // Track current bitfield storage unit
        let mut bf_unit_offset = 0usize; // byte offset of current bitfield storage unit
        let mut bf_bit_pos = 0u32;       // current bit position within the storage unit
        let mut bf_unit_size = 0usize;   // size of the current storage unit in bytes
        let mut in_bitfield = false;

        for field in fields {
            let field_align = field.ty.align();
            let field_size = field.ty.size();
            max_align = max_align.max(field_align);

            if let Some(bw) = field.bit_width {
                if bw == 0 {
                    // Zero-width bitfield: force alignment to next storage unit boundary
                    if in_bitfield {
                        offset = bf_unit_offset + bf_unit_size;
                    }
                    offset = align_up(offset, field_align);
                    in_bitfield = false;
                    bf_bit_pos = 0;
                    // Don't emit a field layout for zero-width bitfields
                    continue;
                }

                let unit_bits = (field_size * 8) as u32;

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
            } else {
                // Regular (non-bitfield) field
                if in_bitfield {
                    offset = bf_unit_offset + bf_unit_size;
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

                offset += field_size;
            }
        }

        // Account for trailing bitfield
        if in_bitfield {
            offset = bf_unit_offset + bf_unit_size;
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

            field_layouts.push(StructFieldLayout {
                name: field.name.clone(),
                offset: 0, // All union fields start at offset 0
                ty: field.ty.clone(),
                bit_offset: None,
                bit_width: None,
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
    /// or a positional index that skips unnamed (anonymous) fields.
    ///
    /// `designator_name`: If Some, look up the field by name.
    /// `current_idx`: The current positional index (used when no designator).
    ///
    /// Returns the resolved field index, or `None` if no valid field found.
    pub fn resolve_init_field_idx(&self, designator_name: Option<&str>, current_idx: usize) -> Option<usize> {
        if let Some(name) = designator_name {
            self.fields.iter().position(|f| f.name == name)
        } else {
            let mut idx = current_idx;
            while idx < self.fields.len() && self.fields[idx].name.is_empty() {
                idx += 1;
            }
            if idx < self.fields.len() { Some(idx) } else { None }
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
            CType::Pointer(_) => 8,
            CType::Array(elem, Some(n)) => elem.size() * n,
            CType::Array(_, None) => 8, // incomplete array treated as pointer
            CType::Function(_) => 8, // function pointer size
            CType::Struct(s) => {
                let layout = StructLayout::for_struct(&s.fields);
                layout.size
            }
            CType::Union(s) => {
                let layout = StructLayout::for_union(&s.fields);
                layout.size
            }
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
            CType::Pointer(_) => 8,
            CType::Array(elem, _) => elem.align(),
            CType::Function(_) => 8,
            CType::Struct(s) | CType::Union(s) => {
                s.fields.iter().map(|f| f.ty.align()).max().unwrap_or(1)
            }
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

    /// Whether this is a floating-point type.
    pub fn is_float(&self) -> bool {
        matches!(self, IrType::F32 | IrType::F64)
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
            CType::Int | CType::Enum(_) => IrType::I32,
            CType::UInt => IrType::U32,
            CType::Long | CType::LongLong => IrType::I64,
            CType::ULong | CType::ULongLong => IrType::U64,
            CType::Float => IrType::F32,
            CType::Double | CType::LongDouble => IrType::F64,
            CType::Pointer(_) | CType::Array(_, _) | CType::Function(_) => IrType::Ptr,
            CType::Struct(_) | CType::Union(_) => IrType::Ptr, // TODO: handle aggregates properly
        }
    }
}
