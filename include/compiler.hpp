#pragma once

#include "cell.hpp"
#include "mcell.hpp"
#include "error.hpp"
#include "macro.hpp"
#include "vector.hpp"

namespace uls {

Cell Compile(Cell expression, const MCell &name_space);

Cell Run_Code(Cell code, bool handle_errors);

// ,CONTINUATION

// One Live_Continuation lives inside the Run_Code function. Together
// with the stack that lives there it keeps track of the current state
// of the virtual machine. Note that this is only one of the three
// forms continuations take. They can be allocated as cells (needed
// for call/cc), they can be a bunch of elements on an MStack (used
// for normal function calling), and this one here is used to
// represent the active continuation.
class Live_Continuation {
  public:
    Live_Continuation(Cell start_code);

    MCell code, reg, arguments, environment;
    Cell function_name; // name of the function we are currently in
    size_t instruction_counter;
};

struct Continuation_Data {
    Cell code, code_counter, continuation, arguments, environment,
        function_name;
};

// This pushing and popping of continuations is slightly crummy,
// information about which field goes where is present here, in the
// Run_Code loop and in the Restore_Continuation function.
constexpr size_t continuation_size = 5;

Cell Make_Continuation(MStack &stack, size_t position, const MCell &parent);

void Restore_Continuation(Cell continuation, MStack &stack);

inline Cell Continuation_Function_Name(Cell continuation) {
    return Extract<Continuation_Data>(continuation).function_name;
}
inline Cell Continuation_Parent(Cell continuation) {
    return Extract<Continuation_Data>(continuation).continuation;
}

inline void Write_Continuation(Cell cell, std::ostream &str, bool display) {
    str << "#<continuation>";
}

// ,COMPILE

// Compiler. Passes over expanded expressions and turns them into code
// vectors.
class Compiler {
  public:
    Compiler(Cell name_space, Cell environment);
    void Compile(Cell expression, bool tail);
    inline Cell Code() const { return Make_Vector(_code); }

  private:
    void Compile_Function_Call(Cell expression, bool tail);
    void Compile_Special_Form(Cell syntax, Cell arguments, bool tail);

    MStack _code;
    MCell _name_space, _environment;
    Cell _defining_symbol;
};

} // namespace uls
