#pragma once

#include <cstdint>
#include "type.hpp"
#include "cell_type.hpp"
#include "error.hpp"

namespace uls {

// ,CELL

// These are a bunch of patterns used to determine the type of
// non-pointer cell types. The top bits of the cell indicate the type.
// When the top bit is 1 it is an integer, when the top two bits are 0
// it is a pointer, etc.

// 1 bit patterns
const uintptr_t int_pattern = 1;
// 2 bit patterns
const uintptr_t compound_pattern = 0;
const uintptr_t fourbit_pattern = 2;
// 4 bit patterns
const uintptr_t symbol_pattern = 2;
const uintptr_t temp_name_pattern = 6;
const uintptr_t instruction_pattern = 10;
const uintptr_t sixbit_pattern = 14;
// 6-bit patterns
const uintptr_t char_pattern = 14;
const uintptr_t instuction_pattern = 30;
const uintptr_t form_pattern = 46;
const uintptr_t special_pattern = 62;

// Some cells have their values defined right here in the enum. These
// are the cells with special values and the fixnums 0 and 1 (it is
// convenient to have those available like this).
//
// The invalid cell is used for a few different purposes, the most
// important purpose is to indicate a value that has not been defined
// yet. User code should not be able to create invalid cells, and
// primitives should never return them. Having invalid cells running
// around in user code will lead to weird results (most likely strange
// errors about a variable being used before it is defined).
typedef uintptr_t Cell;
const Cell false_cell = ((1 << 6) | special_pattern);
const Cell true_cell = ((2 << 6) | special_pattern);
const Cell null_cell = ((3 << 6) | special_pattern);
const Cell void_cell = ((4 << 6) | special_pattern);
const Cell invalid_cell = ((5 << 6) | special_pattern);
const Cell eof_cell = ((256 << 6) | char_pattern);
const Cell zero_cell = int_pattern;
const Cell one_cell = (1 << 1) | int_pattern;

// Some helper functions for encoding and extracting values with the
// top 4 or 6 bits used as type identification.
inline uintptr_t Extract_Fourbit(Cell cell) { return cell >> 4; }
inline Cell Encode_Fourbit(uintptr_t value, uintptr_t pattern) {
    return (value << 4) | pattern;
}
inline bool Match_Fourbit(Cell cell, uintptr_t pattern) {
    return (cell & 15) == pattern;
}
inline uintptr_t Extract_Sixbit(Cell cell) { return cell >> 6; }
inline Cell Encode_Sixbit(uintptr_t value, uintptr_t pattern) {
    return (value << 6) | pattern;
}
inline bool Match_Sixbit(Cell cell, uintptr_t pattern) {
    return (cell & 63) == pattern;
}

// This is the header for a memory block used by a cell. The top two
// are only used by the memory manager, type can be looked at by all
// code (use the Get_Type function), and data contains the data for
// the cell.
struct Cell_Info {
    unsigned short size;
    Pointer_Mask mask;
    Cell_Type type;
    Cell data[1];
};

inline bool Is_Compound(Cell cell) {
    return (reinterpret_cast<uintptr_t &>(cell) & 3) == 0;
}

inline Cell_Info &Compound_Info(Cell cell) {
    S_ASSERT(Is_Compound(cell));
    return *reinterpret_cast<Cell_Info *>(cell);
}

// Get a reference to the guts of a compound cell represented as a
// certain type. Make sure you are actually using the right type with
// the right kind of cell.
template <class Data> inline Data &Extract(const Cell cell) {
    return *reinterpret_cast<Data *>(Compound_Info(cell).data);
}

// Get the type of a compound cell. Does not work on non-compound
// cells!
inline Cell_Type Get_Type(Cell cell) { return Compound_Info(cell).type; }

// ,BIGNUM
// Bignums internally contain:
// - a sign
// - a size
// - a series of 32-bit values that make up the digits of the number in
//   radix 2^32

inline bool Is_Bignum(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == bignum_type;
}

inline bool Is_Fixnum(Cell cell) { return (cell & 1) == int_pattern; }

// ,REAL
// Reals are C++ doubles wrapped up in a cell

inline bool Is_Real(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == real_type;
}

inline double Real_Value(Cell cell) { return Extract<double>(cell); }

// ,RATIONAL
// Rational number are implemented as two integer (fixnum or bignum)
// values. They are always simplified on creation.

struct Rational_Data {
    Cell numerator, denominator;
};

inline bool Is_Rational(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == rational_type;
}

inline Cell &Rational_Numerator(const Cell cell) {
    return Extract<Rational_Data>(cell).numerator;
}

inline Cell &Rational_Denominator(const Cell cell) {
    return Extract<Rational_Data>(cell).denominator;
}

struct Vector_Data {
    Cell size;
    Cell data[1];
};

inline bool Is_Vector(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == vector_type;
}

inline Cell &Vector_Ref(Cell cell, size_t n) {
    S_ASSERT(Is_Vector(cell));
    return Extract<Vector_Data>(cell).data[n];
}

inline bool Is_Inport(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == inport_type;
}
inline bool Is_Outport(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == outport_type;
}

inline bool Is_Namespace(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == namespace_type;
}

inline bool Is_Closure(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == closure_type;
}

inline bool Is_Primitive(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == primitive_type;
}

// ,SYNTAX

// 'Special form' means primitive syntax here - stuff like if, lambda,
// quote are special forms.
inline bool Is_Special_Form(Cell cell) {
    return Match_Sixbit(cell, form_pattern);
}
// Stuff defined by syntax-rules expressions are macros
inline bool Is_Macro(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == macro_type;
}
inline bool Is_Simple_Macro(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == simple_macro_type;
}
inline bool Is_Syntax(Cell cell) {
    return Is_Special_Form(cell) || Is_Macro(cell) || Is_Simple_Macro(cell);
}

// ,CONTINUATION

// These should not show up much on the outside. The things that
// call/cc returns are actually closures with a continuation inside.
// The only way to directly work with these is through the
// #%current_continuation and #%set_continuation instructions.
inline bool Is_Continuation(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == continuation_type;
}

inline bool Is_String(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == string_type;
}

inline bool Is_Renamed_Symbol(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == renamed_symbol_type;
}

inline bool Is_Integer(Cell cell) { return Is_Fixnum(cell) || Is_Bignum(cell); }

inline bool Is_Number(Cell cell) {
    return Is_Integer(cell) || Is_Real(cell) || Is_Rational(cell);
}

// Method to output a cell to an output stream. The display argument
// indicates whether this is a 'display' or a 'write' action
// (influences the way strings and characters are outputted).
void Write(Cell cell, std::ostream &str, bool display = false);

// Some convenience functions related to Write.
inline std::ostream &operator<<(std::ostream &os, Cell cell) {
    Write(cell, os);
    return os;
}
std::string Cell_To_String(Cell cell, bool display = false);

inline bool Is_Symbol(Cell cell) { return Match_Fourbit(cell, symbol_pattern); }

// ,CHARACTER
inline bool Is_Character(Cell cell) { return Match_Sixbit(cell, char_pattern); }
inline Cell Make_Character(int c) { return Encode_Sixbit(c, char_pattern); }
inline int Character_Value(Cell cell) { return Extract_Sixbit(cell); }

// Used to convert cell sizes from 8-bit to 32-bit units
inline uintptr_t Fit_To_Four(uintptr_t num) {
    uintptr_t retval = (num >> 2);
    if ((num & 3) != 0)
        ++retval;
    return retval;
}

inline bool Is_Special(Cell cell) {
    return Match_Sixbit(cell, special_pattern);
}

inline bool Is_Instruction(Cell cell) {
    return Match_Sixbit(cell, instruction_pattern);
}

// ,TEMPNAME
// Temp names are used by the expander to rename local variables,
// which is needed to make hygienic macros behave properly. Sometimes
// it looks like half the code in this file is needed only for those
// awful hygienic macros...

inline Cell Make_Temp_Name(size_t id) {
    return Encode_Fourbit(id, temp_name_pattern);
}
inline bool Is_Temp_Name(Cell cell) {
    return Match_Fourbit(cell, temp_name_pattern);
}
inline size_t Temp_Name_ID(Cell cell) { return Extract_Fourbit(cell); }

// Read from an input port. Takes care to update the line number after
// reading.
Cell Read(Cell inport);

} // namespace uls
