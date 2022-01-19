#pragma once

#include "cell.hpp"
#include "mcell.hpp"

namespace uls {

struct Closure_Data {
    int num_args;
    Cell code, environment, name;
};

Cell Make_Closure(const MCell &code, const MCell &environment, Cell num_args,
                  Cell name);

Cell Make_Closure(const MCell &unfinished_closure, const MCell &environment);

Cell Make_Unfinished_Closure(const MCell &code, Cell num_args, Cell name);

inline Cell Closure_Code(Cell cell) { return Extract<Closure_Data>(cell).code; }

inline Cell Closure_Environment(Cell cell) {
    return Extract<Closure_Data>(cell).environment;
}

inline int Closure_Num_Args(Cell cell) {
    return Extract<Closure_Data>(cell).num_args;
}

inline Cell Closure_Name(Cell cell) { return Extract<Closure_Data>(cell).name; }

}
