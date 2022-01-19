#pragma once

#include <string>
#include "mcell.hpp"

namespace uls {

void Initialize_Syntax(const MCell &name_space);

// Define a top-level symbol to have a certain value.
void Define_Symbol(const MCell &name_space, const std::string &name,
                   const MCell &value);
}
