#pragma once

#include <cstddef>

namespace uls {

// ,CELLTYPE

// Cell types. A cell type is an enum value identifying a compound
// cell type. Non-compound cell types are identified by their patterns
// (see above). All user-defined types are compound. A cell type
// always has a write function associated with it to convert cells of
// that type to text.
enum Cell_Type {
    pair_type = 0,
    vector_type = 1,
    string_type = 2,
    closure_type = 3,
    primitive_type = 4,
    continuation_type = 5,
    inport_type = 6,
    outport_type = 7,
    rational_type = 8,
    real_type = 9,
    bignum_type = 10,
    macro_type = 11,
    simple_macro_type = 12,
    renamed_symbol_type = 13,
    namespace_type = 14,
    moved_cell_type = 15, // used by GC
    available_type = 16
};

// ,CELLINFO

// Used to tell the memory manager which part of a struct contains
// cells, and which part contains other data. Starting from the least
// significant bit, every bit describes a 32-bit part of the cell
// data. 1 means it is a cell and must be examined when collecting
// garbage, 0 means other data. The default mask for Allocate_Cell has
// all bits set to 1, which means all fields are cells. Only the first
// 8 fields can be specified like this. The rest is assumed to be
// non-cell data unless all bits were 1, in which case everything is
// cell data.
typedef std::byte Pointer_Mask;

} // namespace uls
