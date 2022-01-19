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

#include <memory>
#include <vector>

#include "bignum.hpp"
#include "mcell.hpp"
#include "number_io.hpp"
#include "error.hpp"

namespace uls {

constexpr size_t bignum_buffer_size = 400;

class Array_Manager {
  public:
    Array_Manager();
    ~Array_Manager();

    digit *Allocate(int size);

  private:
    void Clear_Temp_Buffers();

    std::unique_ptr<digit> _buffer;
    std::vector<digit *> _temp_buffers;
    size_t _used, _desired_size, _current_size;
};

Array_Manager::Array_Manager()
    : _buffer(new digit[bignum_buffer_size]), _used(0),
      _desired_size(bignum_buffer_size), _current_size(bignum_buffer_size) {}

Array_Manager::~Array_Manager() { Clear_Temp_Buffers(); }

digit *Array_Manager::Allocate(int size) {
    digit *retval = _buffer.get() + _used;
    _used += size;
    if (size > 0 && _used >= _current_size) {
        retval = new digit[size];
        _temp_buffers.push_back(retval);
        _desired_size = _used;
    } else if (_used == 0) {
        Clear_Temp_Buffers();
        if (_desired_size > _current_size) {
            _buffer = std::unique_ptr<digit>(new digit[_desired_size]);
            _current_size = _desired_size;
        }
    }
    return retval;
}

void Array_Manager::Clear_Temp_Buffers() {
    while (!_temp_buffers.empty()) {
        delete[] _temp_buffers.back();
        _temp_buffers.pop_back();
    }
}

digit *Allocate_Array(int size) {
    static Array_Manager manager;
    return manager.Allocate(size);
}

bool Array_Zero(const digit *one, size_t s_one) {
    for (size_t i = 0; i != s_one; ++i) {
        if (one[i] != 0)
            return false;
    }
    return true;
}

bool Array_Smaller(const digit *one, size_t s_one, const digit *two,
                   size_t s_two) {
    int s_big, s_small;
    const digit *big;
    if (s_one < s_two) {
        s_big = s_two - 1;
        s_small = s_one - 1;
        big = two;
    } else {
        s_big = s_one - 1;
        s_small = s_two - 1;
        big = one;
    }
    for (int i = s_big; i != s_small; --i) {
        if (big[i] != 0)
            return (s_one < s_two);
    }
    for (int i = s_small; i != -1; --i) {
        if (one[i] != two[i])
            return one[i] < two[i];
    }
    return false;
}

void Add_Arrays(const digit *one, size_t s_one, const digit *two, size_t s_two,
                digit *result, size_t s_result) {
    size_t carry = 0;
    for (size_t i = 0; i != s_result; ++i) {
        int64 hold = carry;
        if (i < s_one)
            hold += one[i];
        if (i < s_two)
            hold += two[i];

        carry = (hold >> digit_size);
        result[i] = hold;
    }
    S_ASSERT(carry == 0);
}

void Subtract_Arrays(const digit *one, size_t s_one, const digit *two,
                     size_t s_two, digit *result, size_t s_result) {
    int carry = 0;
    for (size_t i = 0; i != s_result; ++i) {
        int64 hold = carry;
        if (i < s_one)
            hold += one[i];
        if (i < s_two)
            hold -= two[i];

        if (hold < 0) {
            hold += digit_radix;
            carry = -1;
        } else {
            carry = 0;
        }
        result[i] = hold;
    }
    S_ASSERT(carry == 0);
}

// Some of this bignum code is quite mind-boggling. There is a bug in
// it that causes it not to work on mac systems (something to do with
// the fact that those machines store bytes in a different order I
// assume). I have to admit I can't quite figure it all out anymore.

void Multiply_Arrays(const digit *one, size_t s_one, const digit *two,
                     size_t s_two, digit *result, size_t s_result) {
    for (size_t i = 0; i != s_result; ++i)
        result[i] = 0;

    for (size_t i = 0; i != s_two; ++i) {
        size_t carry = 0;
        for (size_t j = 0; j < s_one || carry != 0; ++j) {
            uint64 hold =
                (j < s_one) ? static_cast<uint64>(one[j]) * two[i] : 0;
            hold += carry;
            carry = (hold >> digit_size);

            size_t carry2 = hold;
            size_t pos = i + j;
            do {
                S_ASSERT(pos < s_result);
                uint64 hold2 = result[pos];
                hold2 += carry2;
                carry2 = (hold2 >> digit_size);
                result[pos] = hold2;
                ++pos;
            } while (carry2 != 0);
        }
        S_ASSERT(carry == 0);
    }
}

void Divide_Arrays(const digit *one, size_t s_one, const digit *two,
                   size_t s_two, digit *quotient, size_t s_quotient,
                   digit *remain, size_t s_remain) {
    for (size_t i = 0; i != s_remain; ++i) {
        if (i < s_one)
            remain[i] = one[i];
        else
            remain[i] = 0;
    }
    for (size_t i = 0; i != s_quotient; ++i)
        quotient[i] = 0;

    while (!Array_Smaller(remain, s_remain, two, s_two)) {
        size_t hidig_remain = s_remain - 1;
        while (remain[hidig_remain] == 0)
            --hidig_remain;
        size_t hidig_two = s_two - 1;
        while (two[hidig_two] == 0)
            --hidig_two;
        size_t shift = hidig_remain - hidig_two;
        uint64 val_remain = remain[hidig_remain];
        uint64 val_two = two[hidig_two];
        if (hidig_remain != 0) {
            val_remain = digit_radix * val_remain + remain[hidig_remain - 1];
            --shift;
        }
        uint64 guess = (val_remain - 1) / val_two;
        if (guess > digit_mask) {
            guess = (guess >> digit_size);
            ++shift;
        }
        if (guess == 0)
            guess = 1;
        for (size_t j = 0; j != s_two; ++j) {
            int64 carry = guess * two[j];
            size_t pos = j + shift;
            do {
                S_ASSERT(pos < s_remain);
                int64 hold = remain[pos];
                hold -= carry;
                carry = 0;
                if (hold < 0) {
                    carry = ((-hold) >> digit_size) + 1;
                    hold = -((-hold) & digit_mask) + digit_radix;
                }
                remain[pos] = hold;
                ++pos;
            } while (carry != 0);
        }
        uint64 carry = guess, pos = shift;
        do {
            S_ASSERT(pos < s_quotient);
            uint64 hold = quotient[pos];
            hold += carry;
            carry = (hold >> digit_size);
            quotient[pos] = hold;
            ++pos;
        } while (carry != 0);
    }
}

// ,BIGNUM

// Construct a bignum from an array of digits. Will remove trailing
// zero's but will not convert to a fixnum automatically.
Cell Make_Bignum(digit *value, size_t size, bool negative) {
    while (size != 0 && value[size - 1] == 0)
        --size;
    Cell retval =
        Allocate(sizeof(Bignum_Data) - sizeof(digit) + size * sizeof(digit),
                 bignum_type, (std::byte) 0);
    Bignum_Data &data = Extract<Bignum_Data>(retval);
    data.size = size;
    data.negative = negative;
    for (size_t i = 0; i != size; ++i)
        data.data[i] = value[i];
    return retval;
}
// Construct a bignum from a 64-bit int. Will create a bignum even if
// a fixnum would be possible (operations on bignums require two
// bignums, not a bignum and a fixnum).
Cell Make_Bignum(int64 value) {
    bool negative = value < 0;
    if (negative)
        value = -value;
    if (value < static_cast<int64>(digit_radix)) {
        digit val = value;
        return Make_Bignum(&val, 1, negative);
    } else {
        digit val[2];
        val[0] = value;
        val[1] = (value >> digit_size);
        return Make_Bignum(val, 2, negative);
    }
}
// Make an integer from an array of digits. This one will convert to
// fixnum if possible.
Cell Make_Integer(digit *value, size_t size, bool negative) {
    while (size != 0 && value[size - 1] == 0)
        --size;
    if (size == 0)
        return zero_cell;
    else if (size == 1 && value[0] < max_fixnum)
        return Make_Fixnum(negative ? -value[0] : value[0]);
    else
        return Make_Bignum(value, size, negative);
}

// Compare two bignums for equality.
bool Bignum_Equal(Cell one, Cell two) {
    Bignum_Data &d_one = Get_Bignum_Data(one), &d_two = Get_Bignum_Data(two);
    if (d_one.negative != d_two.negative || d_one.size != d_two.size)
        return false;
    for (size_t i = 0; i != d_one.size; ++i) {
        if (d_one.data[i] != d_two.data[i])
            return false;
    }
    return true;
}

// Compare two bignums to see if one is less than two.
bool Bignum_Less(Cell one, Cell two) {
    Bignum_Data &d_one = Get_Bignum_Data(one), &d_two = Get_Bignum_Data(two);
    if (d_one.negative != d_two.negative)
        return d_one.negative;

    bool result = false;
    if (d_one.size != d_two.size)
        result = d_one.size < d_two.size;
    else {
        for (size_t i = d_one.size; i != 0; --i) {
            digit i_one = d_one.data[i], i_two = d_two.data[i];
            if (i_one != i_two) {
                result = i_one < i_two;
                break;
            }
        }
    }

    if (d_one.negative)
        result = -result;
    return result;
}

Cell Integer_Quotient(Cell one, Cell two) {
    S_CHECK(two != zero_cell, "division by zero");
    if (Is_Fixnum(one) && Is_Fixnum(two)) {
        int64 result =
            static_cast<int64>(Fixnum_Value(one)) / Fixnum_Value(two);
        return Make_Integer(result);
    } else {
        MCell m_one = one, m_two = two;
        if (Is_Fixnum(one))
            m_one = Promote_Number(one, n_fixnum);
        else if (Is_Fixnum(two))
            m_two = Promote_Number(two, n_fixnum);

        Bignum_Data &d_one = Get_Bignum_Data(m_one),
                    &d_two = Get_Bignum_Data(m_two);
        size_t s_max = std::max(d_one.size, d_two.size);
        Array_Buffer quotient(s_max), remainder(s_max);
        Divide_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                      quotient.data, quotient.size, remainder.data,
                      remainder.size);
        return Make_Integer(quotient.data, quotient.size,
                            d_one.negative != d_two.negative);
    }
}
Cell Integer_Remainder(Cell one, Cell two) {
    S_CHECK(two != zero_cell, "division by zero");
    if (Is_Fixnum(one) && Is_Fixnum(two)) {
        int64 result =
            static_cast<int64>(Fixnum_Value(one)) % Fixnum_Value(two);
        return Make_Integer(result);
    } else {
        MCell m_one = one, m_two = two;
        if (Is_Fixnum(one))
            m_one = Promote_Number(one, n_fixnum);
        else if (Is_Fixnum(two))
            m_two = Promote_Number(two, n_fixnum);

        Bignum_Data &d_one = Get_Bignum_Data(m_one),
                    &d_two = Get_Bignum_Data(m_two);
        size_t s_max = std::max(d_one.size, d_two.size);
        Array_Buffer quotient(s_max), remainder(s_max);
        Divide_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                      quotient.data, quotient.size, remainder.data,
                      remainder.size);
        return Make_Integer(remainder.data, remainder.size, d_one.negative);
    }
}
Cell Integer_Modulo(Cell one, Cell two) {
    if (Is_Fixnum(one) && Is_Fixnum(two)) {
        int i_one = Fixnum_Value(one), i_two = Fixnum_Value(two);
        int64 result = static_cast<int64>(i_one) % i_two;
        if (result != 0 && (i_one < 0) != (i_two < 0))
            result += i_two;
        return Make_Integer(result);
    } else {
        bool needs_add = Integer_Negative(one) != Integer_Negative(two);
        MCell m_two = two;
        Cell remainder = Integer_Remainder(one, two);
        if (remainder != zero_cell && needs_add)
            remainder = Number_Add(remainder, m_two);
        return remainder;
    }
}

Cell Promote_Number(Cell num, Num_Type type) {
    switch (type) {
    case n_fixnum:
        return Make_Bignum(Fixnum_Value(num));
    case n_bignum:
        return Make_Rational(num, one_cell);
    case n_rational:
        return Make_Real(Number_To_Double(num));
    default:
        S_ASSERT(false);
        return invalid_cell;
    }
}

// Convert any kind of number to a double.
double Number_To_Double(Cell cell) {
    if (Is_Fixnum(cell)) {
        return static_cast<double>(Fixnum_Value(cell));
    } else if (Is_Bignum(cell)) {
        Bignum_Data &data = Get_Bignum_Data(cell);
        double accum = 0;
        for (size_t i = 0; i != data.size; ++i) {
            double add = data.data[i];
            for (size_t j = 0; j != i; ++j)
                add *= digit_radix;
            accum += add;
        }
        if (data.negative)
            accum = -accum;
        return accum;
    } else if (Is_Rational(cell)) {
        return Number_To_Double(Rational_Numerator(cell)) /
               Number_To_Double(Rational_Denominator(cell));
    } else {
        return Real_Value(cell);
    }
}

// Make two numbers have the same type. Takes the types as arguments
// because the caller usually already calculated those. (Talk about
// overactive optimization).
Num_Type Align_Number_Types(Cell &one, Cell &two, Num_Type t_one,
                            Num_Type t_two) {
    MCell m_one = one, m_two = two;
    while (t_one < t_two) {
        m_one = Promote_Number(one, t_one);
        t_one = static_cast<Num_Type>(static_cast<size_t>(t_one) + 1);
    }
    while (t_two < t_one) {
        m_two = Promote_Number(two, t_two);
        t_two = static_cast<Num_Type>(static_cast<size_t>(t_two) + 1);
    }
    one = m_one;
    two = m_two;
    return t_one;
}


// The following functions add, subtract, multiply and divide number
// types. They first make the types the same by promoting the 'lower'
// argument, and then apply the operations that are needed for that
// type of number.
Cell Number_Add(Cell one, Cell two) {
    Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
    if (t_one != t_two)
        t_one = Align_Number_Types(one, two, t_one, t_two);

    switch (t_one) {
    case n_fixnum: {
        // Using 64-bit ints and Make_Integer, the numbers automatically
        // become bignums when they get too fat.
        int64 result =
            static_cast<int64>(Fixnum_Value(one)) + Fixnum_Value(two);
        return Make_Integer(result);
    }
    case n_bignum: {
        Bignum_Data &d_one = Get_Bignum_Data(one),
                    &d_two = Get_Bignum_Data(two);
        Array_Buffer result(std::max(d_one.size, d_two.size) + 1);

        // This is a little icky, but necessary because I use unsigned
        // digits instead of a 2-complement system. An add of a negative
        // and a positive number becomes a subtract.
        if (d_one.negative == d_two.negative) {
            Add_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                       result.data, result.size);
            return Make_Integer(result.data, result.size, d_one.negative);
        } else if (Array_Smaller(d_one.data, d_one.size, d_two.data,
                                 d_two.size)) {
            Subtract_Arrays(d_two.data, d_two.size, d_one.data, d_one.size,
                            result.data, result.size);
            return Make_Integer(result.data, result.size, d_two.negative);
        } else {
            Subtract_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                            result.data, result.size);
            return Make_Integer(result.data, result.size, d_one.negative);
        }
    }
    case n_rational: {
        MCell m_one = one, m_two = two;
        MCell numerator = Number_Multiply(Rational_Numerator(m_one),
                                          Rational_Denominator(m_two));
        MCell temp = Number_Multiply(Rational_Numerator(m_two),
                                     Rational_Denominator(m_one));
        numerator = Number_Add(numerator, temp);
        Cell denominator = Number_Multiply(Rational_Denominator(m_one),
                                           Rational_Denominator(m_two));
        return Make_Simplified_Rational(numerator, denominator);
    }
    case n_real:
        return Make_Real(Number_To_Double(one) + Number_To_Double(two));
    }
    return invalid_cell; // just to silence the compiler
}

Cell Number_Subtract(Cell one, Cell two) {
    Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
    if (t_one != t_two)
        t_one = Align_Number_Types(one, two, t_one, t_two);

    switch (t_one) {
    case n_fixnum: {
        int64 result =
            static_cast<int64>(Fixnum_Value(one)) - Fixnum_Value(two);
        return Make_Integer(result);
    }
    case n_bignum: {
        Bignum_Data &d_one = Get_Bignum_Data(one),
                    &d_two = Get_Bignum_Data(two);
        Array_Buffer result(std::max(d_one.size, d_two.size) + 1);

        if (d_one.negative != d_two.negative) {
            Add_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                       result.data, result.size);
            return Make_Integer(result.data, result.size, d_one.negative);
        } else if (Array_Smaller(d_one.data, d_one.size, d_two.data,
                                 d_two.size)) {
            Subtract_Arrays(d_two.data, d_two.size, d_one.data, d_one.size,
                            result.data, result.size);
            return Make_Integer(result.data, result.size, !d_one.negative);
        } else {
            Subtract_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                            result.data, result.size);
            return Make_Integer(result.data, result.size, d_one.negative);
        }
    }
    case n_rational: {
        MCell m_one = one, m_two = two;
        MCell numerator = Number_Multiply(Rational_Numerator(m_one),
                                          Rational_Denominator(m_two));
        MCell temp = Number_Multiply(Rational_Numerator(m_two),
                                     Rational_Denominator(m_one));
        numerator = Number_Subtract(numerator, temp);
        Cell denominator = Number_Multiply(Rational_Denominator(m_one),
                                           Rational_Denominator(m_two));
        return Make_Simplified_Rational(numerator, denominator);
    }
    case n_real:
        return Make_Real(Number_To_Double(one) - Number_To_Double(two));
    }
    return invalid_cell;
}

Cell Number_Multiply(Cell one, Cell two) {
    Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
    if (t_one != t_two)
        t_one = Align_Number_Types(one, two, t_one, t_two);

    switch (t_one) {
    case n_fixnum: {
        int64 result =
            static_cast<int64>(Fixnum_Value(one)) * Fixnum_Value(two);
        return Make_Integer(result);
    }
    case n_bignum: {
        Bignum_Data &d_one = Get_Bignum_Data(one),
                    &d_two = Get_Bignum_Data(two);
        Array_Buffer result(d_one.size + d_two.size);
        Multiply_Arrays(d_one.data, d_one.size, d_two.data, d_two.size,
                        result.data, result.size);
        return Make_Integer(result.data, result.size,
                            d_one.negative != d_two.negative);
    }
    case n_rational: {
        MCell m_one = one, m_two = two;
        MCell numerator = Number_Multiply(Rational_Numerator(m_one),
                                          Rational_Numerator(m_two));
        Cell denominator = Number_Multiply(Rational_Denominator(m_one),
                                           Rational_Denominator(m_two));
        return Make_Simplified_Rational(numerator, denominator);
    }
    case n_real:
        return Make_Real(Number_To_Double(one) * Number_To_Double(two));
    }
    return invalid_cell;
}

Cell Number_Divide(Cell one, Cell two) {
    Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
    // Divide for integers means becoming a rational number
    if ((t_one == n_fixnum || t_one == n_bignum) &&
        (t_two == n_fixnum || t_two == n_bignum))
        return Make_Simplified_Rational(one, two);

    if (t_one != t_two)
        t_one = Align_Number_Types(one, two, t_one, t_two);

    if (t_one == n_rational) {
        MCell m_one = one, m_two = two;
        MCell numerator = Number_Multiply(Rational_Numerator(m_one),
                                          Rational_Denominator(m_two));
        Cell denominator = Number_Multiply(Rational_Denominator(m_one),
                                           Rational_Numerator(m_two));
        return Make_Simplified_Rational(numerator, denominator);
    } else {
        return Make_Real(Number_To_Double(one) / Number_To_Double(two));
    }
}


Cell Make_Real(double value) {
    Cell retval = Allocate_Cell<double>(real_type, (std::byte) 0);
    Extract<double>(retval) = value;
    return retval;
}


Cell Greatest_Common_Divisor(Cell one, Cell two) {
    S_ASSERT(Is_Integer(one) && Is_Integer(two));
    MCell a = one, b = two;
    if (Integer_Less(b, a))
        std::swap<Cell>(a, b);

    while (a != zero_cell) {
        b = Integer_Remainder(b, a);
        std::swap<Cell>(a, b);
    }
    return b;
}


// Make as simple a rational possible from two integers. Will return
// an integer if the denominator ends up as one.
Cell Make_Simplified_Rational(Cell numerator, Cell denominator) {
    S_ASSERT(Is_Integer(numerator) && Is_Integer(denominator));

    if (numerator == zero_cell)
        return zero_cell;
    S_CHECK(denominator != zero_cell,
            "invalid rational number (denominator equals 0)");

    MCell num = numerator, denom = denominator;
    if (Integer_Negative(denom)) {
        num = Number_Subtract(zero_cell, num);
        denom = Number_Subtract(zero_cell, denom);
    }

    MCell common = Greatest_Common_Divisor(num, denom);
    if (common != one_cell) {
        num = Integer_Quotient(num, common);
        denom = Integer_Quotient(denom, common);
    }

    if (denom == one_cell)
        return num;
    else
        return Make_Rational(num, denom);
}

// A 'tower' system for numbers, the number types form a hierarchy an
// can be promoted upwards.
Num_Type Number_Type(Cell cell) {
    S_ASSERT(Is_Number(cell));

    if (Is_Fixnum(cell)) {
        return n_fixnum;
    } else {
        S_ASSERT(Is_Compound(cell));
        Cell_Type type = Get_Type(cell);
        if (type == bignum_type)
            return n_bignum;
        else if (type == rational_type)
            return n_rational;
        else
            return n_real;
    }
}

} // namespace uls
