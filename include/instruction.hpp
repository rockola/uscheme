#pragma once

#include <string>
#include "cell.hpp"

namespace uls {

// Construct an enum of instructions with some macro magic.
#define INSTRUCTION(name) i_##name,
enum Instruction {
#include "../src/instructions.i"
    i_last
};
#undef INSTRUCTION


// A table associating instructions with their name is needed for
// reading and writing instructions.
struct Instruction_Info {
    Instruction instruction;
    std::string name;
};

Instruction_Info *Instruction_List();
Instruction_Info &Find_Instruction(const std::string &name);
Instruction_Info &Find_Instruction(Instruction instruction);

#undef INSTRUCTION

inline Cell Make_Instruction(Instruction ins) {
    return Encode_Sixbit(reinterpret_cast<uintptr_t &>(ins),
                         instruction_pattern);
}

inline Instruction Instruction_Value(Cell cell) {
    size_t temp = Extract_Sixbit(cell);
    return reinterpret_cast<Instruction &>(temp);
}

} // namespace uls
