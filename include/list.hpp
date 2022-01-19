#pragma once

#include "cell.hpp"
#include "mcell.hpp"

namespace uls {

bool Member(Cell value, Cell list);
Cell Assoc(Cell needle, Cell list);

size_t List_Length(Cell list, const char *error = "improper list");
bool Is_Proper_List(Cell list);

Cell Append_To_List(Cell cell, Cell lst);

void Integrate_Tree(Cell element, MCell &tree);
} // namespace uls
