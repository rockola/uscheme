#pragma once

#include "cell.hpp"
#include "mcell.hpp"

namespace uls {

// ,VECTOR

// Some different ways of constructing vectors
Cell Make_Vector(size_t size, Cell fill = null_cell);
Cell Make_Vector(const MStack &stack);
Cell Make_Vector_From_List(Cell list);

} // namespace uls
