#pragma once

#include <iostream>
#include "cell.hpp"
#include "mcell.hpp"
#include "closure.hpp"
#include "primitive.hpp"

namespace uls {

// ,PORT

// Ports made with a filename have to be closed before they are
// collected or they will leak memory.
Cell Make_Inport(const MCell &filename);

Cell Make_Outport(const MCell &filename);

Cell Make_Inport(std::istream &stream);

Cell Make_Outport(std::ostream &stream);

Cell Inport_Read_Char(Cell port);

Cell Inport_Peek_Char(Cell port);

bool Inport_Ready(Cell port);

void Reopen_Inport(Cell port);

void Reopen_Outport(Cell port);

struct Inport_Data {
    Cell file_name;
    std::istream *stream;
    size_t position, line;
};

struct Outport_Data {
    Cell file_name;
    std::ostream *stream;
};

std::istream &Inport_Stream(Cell port);

std::ostream &Outport_Stream(Cell cell);

size_t Inport_Line(Cell port);

inline bool Inport_Is_Open(Cell cell) {
    S_ASSERT(Is_Inport(cell));
    return Extract<Inport_Data>(cell).stream != nullptr;
}

inline bool Outport_Is_Open(Cell cell) {
    S_ASSERT(Is_Outport(cell));
    return Extract<Outport_Data>(cell).stream != nullptr;
}

void Close_Inport(Cell cell);

void Close_Outport(Cell cell);

void Write_Inport(Cell cell, std::ostream &str, bool display);

void Write_Outport(Cell cell, std::ostream &str, bool display);

// Read a cell from a stream.
Cell Read(std::istream &stream);

inline std::istream &operator>>(std::istream &is, Cell &cell) {
    cell = Read(is);
    return is;
}

inline void Write_Primitive(Cell cell, std::ostream &str, bool display) {
    str << "#<primitive:" << (unsigned)Primitive_Name(cell) << '>';
}

inline void Write_Closure(Cell cell, std::ostream &str, bool display) {
    str << "#<closure";
    Cell name = Closure_Name(cell);
    if (Is_Symbol(name))
        str << ':' << Symbol_Name(name);
    str << '>';
}

inline std::string Function_Name(Cell name) {
    if (name == false_cell) {
        return "unnamed function";
    } else {
        S_ASSERT(Is_Symbol(name));
        return Symbol_Name(name);
    }
}

inline void Write_Namespace(Cell cell, std::ostream &str, bool display) {
    str << "#<namespace>";
}

// ,RATIONAL

inline void Write_Rational(Cell cell, std::ostream &str, bool display) {
    S_ASSERT(Is_Rational(cell));
    str << (unsigned)Rational_Numerator(cell) << '/'
        << (unsigned)Rational_Denominator(cell);
}

void Write_Pair(Cell cell, std::ostream &str, bool display);

// Displaying macro's should not actually happen (synax can not be
// dereferenced), but it is nice to have something comprehensive
// instead of a segfault when it happens.
inline void Write_Macro(Cell cell, std::ostream &str, bool display) {
    str << "#<macro>";
}

void Write_Vector(Cell cell, std::ostream &str, bool display);

void Write_Bignum(Cell cell, std::ostream &str, bool display);

} // namespace uls
