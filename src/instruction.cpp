#include "instruction.hpp"
#include "error.hpp"

namespace uls {

// ,INSTRUCTION

// instructions.i is included again, but this time to build the table.
#define INSTRUCTION(name) {i_##name, #name},
Instruction_Info *Instruction_List() {
    static Instruction_Info instruction_info[] = {
#include "instructions.i"
        {i_last, ""}};
    return instruction_info;
}

Instruction_Info &Find_Instruction(const std::string &name) {
    for (Instruction_Info *cur = Instruction_List(); cur->instruction != i_last;
         ++cur) {
        if (cur->name == name)
            return *cur;
    }
    throw Scheme_Error("unknown instruction name: " + name);
}
Instruction_Info &Find_Instruction(Instruction instruction) {
    for (Instruction_Info *cur = Instruction_List(); cur->instruction != i_last;
         ++cur) {
        if (cur->instruction == instruction)
            return *cur;
    }
    S_THROW(std::runtime_error, "unknown instruction");
}

} // namespace uls
