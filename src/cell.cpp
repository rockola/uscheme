#include "cell.hpp"
#include "interpreter.hpp"
#include "primitive.hpp"
#include "instruction.hpp"
#include "output.hpp"
#include "inputsplitter.hpp"

namespace uls {

// Write a cell to a stream. Display indicates whether it is printed
// in s-expression or pretty format (with display on strings have no
// "'s and characters have no #\)
void Write(Cell cell, std::ostream &str, bool display) {
    // Compounds get printed with the function that is associated with their
    // type
    if (Is_Compound(cell)) {
        Interpreter::Get_Function(Get_Type(cell))(cell, str, display);
    }
    // Non-compounds get handled by this function
    else if (Is_Symbol(cell)) {
        bool normal_symbol = false;
        std::string name = Symbol_Name(cell);
        // Check whether the symbol contains any weird characters.
        if ((name.size() > 0 && Is_Symbol_Start(name[0])) ||
            (Is_Ambiguous_Char(name[0]) && name.size() == 1) || name == "...") {
            for (std::string::iterator i = name.begin() + 1; i != name.end();
                 ++i) {
                if (!Is_Symbol_Char(*i))
                    goto outside;
            }
            normal_symbol = true;
        }
    outside:
        // add pipes around weird symbols
        if (!normal_symbol && !display)
            str << '|';
        str << name;
        if (!normal_symbol && !display)
            str << '|';
    } else if (Is_Special(cell)) {
        if (cell == null_cell)
            str << "()";
        else if (cell == true_cell)
            str << "#t";
        else if (cell == false_cell)
            str << "#f";
        else if (cell == void_cell)
            str << "#v";
        else if (cell == invalid_cell)
            str << "#<INVALID>";
        else
            S_ASSERT(false);
    } else if (Is_Fixnum(cell)) {
        str << Fixnum_Value(cell);
    } else if (Is_Character(cell)) {
        int c = Character_Value(cell);
        if (!display)
            str << "#\\";
        if (c == '\n' && !display)
            str << "newline";
        else if (c == ' ' && !display)
            str << "space";
        else if (c == 256 && !display)
            str << "eof";
        else if (c == 256) // eof always keeps its #\.
            str << "#\\eof";
        else
            str << static_cast<char>(c);
    } else if (Is_Instruction(cell)) {
        str << "#%" << Find_Instruction(Instruction_Value(cell)).name;
    } else if (Is_Special_Form(cell)) {
        str << "#<special form>";
    } else if (Is_Temp_Name(cell)) {
        str << "#<temp:" << (unsigned)Temp_Name_ID(cell) << '>';
    } else {
        S_ASSERT(false);
    }
}

std::string Cell_To_String(Cell cell, bool display) {
    std::ostringstream result;
    Write(cell, result, display);
    return result.str();
}

// Read from an input port. Takes care to update the line number after
// reading.
Cell Read(Cell inport) {
    S_CHECK(Inport_Is_Open(inport), "can not read from closed port");
    MCell port = inport;
    Stream_Input_Splitter splitter(*Extract<Inport_Data>(port).stream);
    try {
        Cell retval = splitter.Read();
        Extract<Inport_Data>(port).line += splitter.Lines_Read();
        return retval;
    } catch (Scheme_Error &e) {
        Extract<Inport_Data>(port).line += splitter.Lines_Read();
        throw;
    }
}

} // namespace uls
