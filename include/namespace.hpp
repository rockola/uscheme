#pragma once

#include <string>
#include "cell.hpp"
#include "mcell.hpp"
#include "bignum.hpp"
#include "primitive.hpp"

namespace uls {

struct Namespace_Data {
    Cell size;
    Cell parent;
    Cell binding[1];
};

constexpr size_t default_workspace_size = 503;
constexpr size_t null_env_size = 23;
constexpr size_t report_env_size = 233;

Cell Get_Binding(const MCell &name_space, Cell symbol);

size_t Hash_Symbol(Cell symbol, Cell name_space);

Cell &Namespace_Ref(Cell space, size_t index);

Cell Namespace_Parent(Cell space);

size_t Namespace_Size(Cell space);

Cell Make_Namespace(const MCell &parent, size_t size);

void Bootstrap_Namespace(const MCell &name_space);

void Define_Symbol(const MCell &name_space, const std::string &name,
                   const MCell &value);

Cell Resolve_Name(Cell identifier, Cell env, Cell name_space, bool syntax_value,
                  bool one_deep = false);

// These are used to represent offsets in an environment (list of
// vectors). Depth indicates which vector, distance indicates the
// element of that vector.
struct Environment_Ref {
    unsigned short depth, distance;
};
const unsigned short not_found = max_short;

Environment_Ref Find_In_Environment(Cell symbol, Cell environment);

// This is how refs are represented in code vectors (pair of fixnums).
inline Cell Make_Env_Ref(size_t depth, size_t distance) {
    return XCons(Make_Fixnum(depth), Make_Fixnum(distance));
}

unsigned short Environment_Offset(Cell symbol, Cell env_vector);

} // namespace uls
