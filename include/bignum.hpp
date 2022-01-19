/* Copyright (c) 2004 Marijn Haverbeke
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any
 * damages arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose, including commercial applications, and to alter it and
 * redistribute it freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must
 *    not claim that you wrote the original software. If you use this
 *    software in a product, an acknowledgment in the product
 *    documentation would be appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must
 *    not be misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 * Marijn Haverbeke
 * marijn(at)haverbeke.nl
 */

#pragma once

#include "type.hpp"
#include "cell.hpp"

// Methods for operations on bignums. The interface of passing
// pointers and sizes is a little clunky but this was necessary
// because there are two representations of bignums - inside cells and
// as Array_Buffers.

namespace uls {

// The digit type and some conts to make messing with it easier.
typedef unsigned int digit;
const size_t digit_size = byte_size * sizeof(digit);
const uint64 digit_radix = (static_cast<uint64>(1) << digit_size),
             digit_mask = digit_radix - 1;

// Buffers to keep bignums in during calculations.
digit *Allocate_Array(int size);
struct Array_Buffer {
    explicit Array_Buffer(int s) : size(s), data(Allocate_Array(size)) {}
    ~Array_Buffer() { Allocate_Array(-size); }
    size_t size;
    digit *data;
};

// The operations. Add and subtract allow the result buffer to be the
// same as one of the source buffers, with multiply and divide this
// does not work.
bool Array_Zero(const digit *one, size_t s_one);
bool Array_Smaller(const digit *one, size_t s_one, const digit *two,
                   size_t s_two);
void Add_Arrays(const digit *one, size_t s_one, const digit *two, size_t s_two,
                digit *result, size_t s_result);
void Subtract_Arrays(const digit *one, size_t s_one, const digit *two,
                     size_t s_two, digit *result, size_t s_result);
void Multiply_Arrays(const digit *one, size_t s_one, const digit *two,
                     size_t s_two, digit *result, size_t s_result);
void Divide_Arrays(const digit *one, size_t s_one, const digit *two,
                   size_t s_two, digit *quotient, size_t s_quotient,
                   digit *remain, size_t s_remain);

Cell Make_Bignum(int64 value);

bool Bignum_Equal(Cell one, Cell two);

// Bignums are tricky to work with, here are some basic numeric
// operations that can be applied to them (more can be found in
// ,NUMBER)

Cell Integer_Quotient(Cell one, Cell two);
Cell Integer_Remainder(Cell one, Cell two);
Cell Integer_Modulo(Cell one, Cell two);

inline int Fixnum_Value(Cell cell) {
    S_ASSERT(Is_Fixnum(cell));
    uintptr_t bits = cell >> 1;
    // This is needed to restore the sign, it basically takes the
    // almost-most-significant bit and copies it to the most significant
    // bit (which got trampled by the shifting)
    bits |= ((bits & (1 << (sizeof(int) * byte_size - 2))) << 1);
    return reinterpret_cast<int &>(bits);
}

// Bignums are stored as a set of unsigned digits with an extra field
// for the sign.
struct Bignum_Data {
    unsigned short size;
    bool negative;
    digit data[1];
};

inline Bignum_Data &Get_Bignum_Data(Cell cell) {
    S_ASSERT(Is_Bignum(cell));
    return Extract<Bignum_Data>(cell);
}

bool Bignum_Less(Cell one, Cell two);

// Some operations that work on both bignums and fixnums.
inline bool Integer_Negative(Cell cell) {
    if (Is_Fixnum(cell))
        return Fixnum_Value(cell) < 0;
    else
        return Get_Bignum_Data(cell).negative;
}
inline bool Integer_Equal(Cell one, Cell two) {
    bool small = Is_Fixnum(one);
    if (small != Is_Fixnum(two))
        return false;
    else if (small)
        return one == two;
    else
        return Bignum_Equal(one, two);
}

inline bool Integer_Less(Cell one, Cell two) {
    bool one_small = Is_Fixnum(one), two_small = Is_Fixnum(two);
    if (one_small && !two_small)
        return true;
    else if (!one_small && two_small)
        return false;
    else if (one_small)
        return Fixnum_Value(one) < Fixnum_Value(two);
    else
        return Bignum_Less(one, two);
}

inline Cell Make_Fixnum(int value) {
    S_ASSERT(std::abs(value) < max_fixnum);
    return (value << 1) | int_pattern;
}

Cell Make_Integer(digit *value, size_t size, bool negative);

inline Cell Make_Integer(int64 value) {
    uint64 abs_value = (value < 0) ? -value : value;
    if (abs_value >= max_fixnum)
        return Make_Bignum(value);
    else
        return Make_Fixnum(value);
}

// The Num_Type system is used to conveniently 'promote' numbers to
// other number types.
enum Num_Type { n_fixnum = 0, n_bignum = 1, n_rational = 2, n_real = 3 };

Num_Type Number_Type(Cell cell);

// Be careful with Promote_Number, promoting fixnums leads to bignums
// that should be fixnums and promoting bignums leads to rationals
// that have a denominator of 1 - other procedures can get confused by
// such objects.
Cell Promote_Number(Cell num, Num_Type type);

// Get the double value of any type of number, can be convenient with
// all those different types confusing your code.
double Number_To_Double(Cell cell);

// Basic operations on any kind of number cells.
Cell Number_Add(Cell one, Cell two);
Cell Number_Subtract(Cell one, Cell two);
Cell Number_Multiply(Cell one, Cell two);
Cell Number_Divide(Cell one, Cell two);

Cell Make_Real(double value);

Cell Make_Simplified_Rational(Cell numerator, Cell denominator);

} // namespace uls
