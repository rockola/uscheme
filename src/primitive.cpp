#include <cmath>
#include <fstream>
#include "cell.hpp"
#include "bignum.hpp"
#include "inputsplitter.hpp"
#include "number_io.hpp"
#include "primitive.hpp"
#include "string.hpp"
#include "vector.hpp"
#include "interpreter.hpp"
#include "output.hpp"
#include "namespace.hpp"
#include "compiler.hpp"

namespace uls {

// Convenience function to make a scheme-boolean out of a c boolean.
inline Cell Make_Bool(bool value) { return value ? true_cell : false_cell; }

inline bool Renamed_Symbol_Equal(Cell one, Cell two) {
    Renamed_Symbol_Data &d_one = Extract<Renamed_Symbol_Data>(one),
                        &d_two = Extract<Renamed_Symbol_Data>(two);
    return d_one.old_name == d_two.old_name &&
           d_one.unique_value == d_two.unique_value;
}

inline double Truncate_Double(double value) {
    if (value < 0)
        return std::ceil(value);
    else
        return std::floor(value);
}

// Convert a double to an integer (fixnum if possible)
Cell Double_To_Integer(double value) {
    bool negative = value < 0;
    if (negative)
        value = -value;
    value = std::floor(value + .5);

    std::vector<digit> array;
    while (value != 0) {
        double quotient = std::floor(value / digit_radix);
        double remainder = value - (quotient * digit_radix);
        array.push_back(static_cast<digit>(remainder + .5));
        value = quotient;
    }

    return Make_Integer(&array[0], array.size(), negative);
}

// Because equal is used in C++ code quite a bit I implemented it in
// C++ instead of scheme.
bool Equal(Cell one, Cell two) {
    if (one == two) {
        return true;
    } else if (Is_Compound(one) && Is_Compound(two)) {
        Cell_Type type = Get_Type(one);
        if (Get_Type(two) != type) {
            return false;
        } else if (type == pair_type) {
            return Equal(Car(one), Car(two)) && Equal(Cdr(one), Cdr(two));
        } else if (type == vector_type) {
            if (Vector_Size(one) == Vector_Size(two)) {
                size_t size = Vector_Size(one);
                for (size_t i = 0; i != size; ++i) {
                    if (!Equal(Vector_Ref(one, i), Vector_Ref(two, i)))
                        return false;
                }
                return true;
            }
        } else if (type == string_type) {
            if (String_Size(one) == String_Size(two)) {
                size_t size = String_Size(one);
                for (size_t i = 0; i != size; ++i) {
                    if (String_Ref(one, i) != String_Ref(two, i))
                        return false;
                }
                return true;
            }
        } else if (type == rational_type) {
            return Equal(Rational_Numerator(one), Rational_Numerator(two)) and
                   Equal(Rational_Denominator(one), Rational_Denominator(two));
        } else if (type == real_type) {
            return Real_Value(one) == Real_Value(two);
        } else if (type == bignum_type) {
            return Bignum_Equal(one, two);
        } else if (type == renamed_symbol_type) {
            return Renamed_Symbol_Equal(one, two);
        }
    }
    return false;
}

// The primitives are mostly a lot of really simple functions
// implemented in terms of other functions in really straightforward
// ways.

// general

Cell p_eqp(Cell one, Cell two) { return Make_Bool(one == two); }

Cell p_eqvp(Cell one, Cell two) {
    if (Is_Bignum(one) && Is_Bignum(two))
        return Make_Bool(Bignum_Equal(one, two));
    else if (Is_Real(one) && Is_Real(two))
        return Make_Bool(Real_Value(one) == Real_Value(two));
    else if (Is_Rational(one) && Is_Rational(two))
        return Make_Bool(
            Integer_Equal(Rational_Numerator(one), Rational_Numerator(two)) and
            Integer_Equal(Rational_Denominator(one),
                          Rational_Denominator(two)));
    else
        return Make_Bool(one == two);
}

Cell p_equalp(Cell one, Cell two) { return Make_Bool(Equal(one, two)); }

std::string Make_Arg_Type_Error(const std::string &function,
                                const std::string &num,
                                const std::string &type) {
    static const char *arg = "argument ", *to = " to ",
                      *must = " must be of type ";
    return arg + num + to + function + must + type;
}

// pairs and lists

#define CHECK_N_TYPE(var, type, function, num)                                 \
    S_CHECK(Is_##type(var), Make_Arg_Type_Error(function, #num, #type))

Cell p_car(Cell pair) {
    CHECK_N_TYPE(pair, Pair, "car", 1);
    return Car(pair);
}

Cell p_cdr(Cell pair) {
    CHECK_N_TYPE(pair, Pair, "cdr", 1);
    return Cdr(pair);
}

Cell p_cons(Cell car, Cell cdr) { return Cons(car, cdr); }

Cell p_nullp(Cell cell) { return Make_Bool(cell == null_cell); }
Cell p_pairp(Cell cell) { return Make_Bool(Is_Pair(cell)); }
Cell p_listp(Cell cell) {
    Cell runner_one = cell, runner_two = cell;
    while (Is_Pair(runner_two)) {
        runner_one = Cdr(runner_one);
        runner_two = Cdr(runner_two);
        if (!Is_Pair(runner_two))
            break;
        runner_two = Cdr(runner_two);
        if (runner_one == runner_two)
            return false_cell;
    }
    return Make_Bool(runner_two == null_cell);
}

Cell p_set_car(Cell pair, Cell value) {
    CHECK_N_TYPE(pair, Pair, "set-car!", 1);
    Car(pair) = value;
    return void_cell;
}
Cell p_set_cdr(Cell pair, Cell value) {
    CHECK_N_TYPE(pair, Pair, "set-cdr!", 1);
    Cdr(pair) = value;
    return void_cell;
}

// numbers

inline bool Double_Is_Int(double value) { return std::floor(value) == value; }

Cell p_numberp(Cell cell) { return Make_Bool(Is_Number(cell)); }
// To make things simple, the concept integer in the C++ code means a
// Bignum or Fixnum, while in scheme it can also be a Real that
// contains an integer value.
Cell p_integerp(Cell cell) {
    return Make_Bool(Is_Integer(cell) ||
                     (Is_Real(cell) && Double_Is_Int(Real_Value(cell))));
}
Cell p_rationalp(Cell cell) {
    return Make_Bool(Is_Integer(cell) || Is_Rational(cell));
}
Cell p_realp(Cell cell) { return Make_Bool(Is_Number(cell)); }

Cell p_exactp(Cell cell) {
    CHECK_N_TYPE(cell, Number, "exact?", 1);
    return Make_Bool(!Is_Real(cell));
}
Cell p_inexactp(Cell cell) {
    CHECK_N_TYPE(cell, Number, "inexact?", 1);
    return Make_Bool(Is_Real(cell));
}

// Some stuff to make error checking easier. These are a little odd
// (especially the type system of CHECK_TYPE), but the idea is to have
// as little clutter as possible in the primitives and to minimize the
// amount of strings that have to be kept in the executable.

#define CHECK_TYPE(var, type, num, function_name)                                     \
    S_CHECK(Is_##type(var), Make_Arg_Type_Error(function_name, #num, #type))
inline bool Single_Optional_Argument(Cell optional, const char *function) {
    if (optional == null_cell)
        return false;
    S_CHECK(Cdr(optional) == null_cell,
            "too many arguments to " + std::string(function));
    return true;
}

Cell p_equals(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "=");
    CHECK_TYPE(two, Number, 2, "=");
    return Make_Bool(Number_To_Double(one) == Number_To_Double(two));
}
Cell p_less(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "<");
    CHECK_TYPE(two, Number, 2, "<");
    return Make_Bool(Number_To_Double(one) < Number_To_Double(two));
}
Cell p_less_equal(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "<=");
    CHECK_TYPE(two, Number, 2, "<=");
    return Make_Bool(Number_To_Double(one) <= Number_To_Double(two));
}
Cell p_greater(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, ">");
    CHECK_TYPE(two, Number, 2, ">");
    return Make_Bool(Number_To_Double(one) > Number_To_Double(two));
}
Cell p_greater_equal(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, ">=");
    CHECK_TYPE(two, Number, 2, ">=");
    return Make_Bool(Number_To_Double(one) >= Number_To_Double(two));
}

Cell p_plus(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "+");
    CHECK_TYPE(two, Number, 2, "+");
    return Number_Add(one, two);
}
Cell p_minus(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "-");
    CHECK_TYPE(two, Number, 2, "-");
    return Number_Subtract(one, two);
}
Cell p_times(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "*");
    CHECK_TYPE(two, Number, 2, "*");
    return Number_Multiply(one, two);
}
Cell p_divide(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "/");
    CHECK_TYPE(two, Number, 2, "/");
    return Number_Divide(one, two);
}

bool OPTIONAL(Cell var, const char* function_name) {
    return Single_Optional_Argument(var, function_name);
}

Cell p_number_to_string(Cell number, Cell custom_radix) {
    CHECK_TYPE(number, Number, 1, "number->string");
    int radix = 10;
    if (OPTIONAL(custom_radix, "number->string")) {
        CHECK_TYPE(Car(custom_radix), Fixnum, 2, "number->string");
        radix = Fixnum_Value(Car(custom_radix));
        S_CHECK(radix > 1 && radix < 21,
                "radix for number->string can only range from 2 to 20");
    }
    return Make_String(Number_To_String(number, radix));
}
Cell p_string_to_number(Cell str, Cell custom_radix) {
    CHECK_TYPE(str, String, 1, "string->number");
    int radix = 10;
    if (OPTIONAL(custom_radix, "string->number")) {
        CHECK_TYPE(Car(custom_radix), Fixnum, 2, "string->number");
        radix = Fixnum_Value(Car(custom_radix));
        S_CHECK(radix > 1 && radix < 21,
                "radix for string->number can only range from 2 to 20");
    }
    try {
        return String_To_Number(String_Value(str), radix);
    } catch (const Scheme_Error &e) {
        return false_cell;
    }
}

Cell p_quotient(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "quotient");
    CHECK_TYPE(two, Number, 2, "quotient");
    if (Is_Integer(one) && Is_Integer(two)) {
        return Integer_Quotient(one, two);
    } else {
        double val_one = Number_To_Double(one), val_two = Number_To_Double(two);
        S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two),
                "arguments to quotient must be integers");
        return Make_Real(Truncate_Double(val_one / val_two));
    }
}
Cell p_remainder(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "remainder");
    CHECK_TYPE(two, Number, 2, "remainder");
    if (Is_Integer(one) && Is_Integer(two)) {
        return Integer_Remainder(one, two);
    } else {
        double val_one = Number_To_Double(one), val_two = Number_To_Double(two);
        S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two),
                "arguments to remainder must be integers");
        return Make_Real(val_one -
                         val_two * Truncate_Double(val_one / val_two));
    }
}
Cell p_modulo(Cell one, Cell two) {
    CHECK_TYPE(one, Number, 1, "modulo");
    CHECK_TYPE(two, Number, 2, "modulo");
    if (Is_Integer(one) && Is_Integer(two)) {
        return Integer_Modulo(one, two);
    } else {
        double val_one = Number_To_Double(one), val_two = Number_To_Double(two);
        S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two),
                "arguments to modulo must be integers");
        double result = val_one - val_two * Truncate_Double(val_one / val_two);
        if (result != 0 && (val_one < 0) != (val_two < 0))
            result += val_two;
        return Make_Real(result);
    }
}

Cell p_numerator(Cell num) {
    if (Is_Integer(num))
        return num;
    else if (Is_Rational(num))
        return Rational_Numerator(num);
    else
        throw Scheme_Error("argument to numerator must be a rational number");
}
Cell p_denominator(Cell num) {
    if (Is_Integer(num))
        return one_cell;
    else if (Is_Rational(num))
        return Rational_Denominator(num);
    else
        throw Scheme_Error("argument to denominator must be a rational number");
}

Cell p_ceiling(Cell num) {
    CHECK_N_TYPE(num, Number, "ceiling", 1);
    if (Is_Integer(num))
        return num;
    else {
        double value = std::ceil(Number_To_Double(num));
        if (Is_Rational(num))
            return Double_To_Integer(value);
        else
            return Make_Real(value);
    }
}
Cell p_floor(Cell num) {
    CHECK_N_TYPE(num, Number, "floor", 1);
    if (Is_Integer(num))
        return num;
    else {
        double value = std::floor(Number_To_Double(num));
        if (Is_Rational(num))
            return Double_To_Integer(value);
        else
            return Make_Real(value);
    }
}
Cell p_truncate(Cell num) {
    CHECK_N_TYPE(num, Number, "truncate", 1);
    if (Is_Integer(num))
        return num;
    else {
        double value = Truncate_Double(Number_To_Double(num));
        if (Is_Rational(num))
            return Double_To_Integer(value);
        else
            return Make_Real(value);
    }
}
Cell p_round(Cell num) {
    CHECK_N_TYPE(num, Number, "round", 1);
    if (Is_Integer(num))
        return num;
    else {
        double value = Number_To_Double(num);
        bool negative = value < 0;
        if (negative)
            value = -value;
        double floor = std::floor(value), fraction = value - floor;
        if (fraction < .5)
            value = floor;
        else if (fraction > .5)
            value = floor + 1;
        else if (Double_Is_Int(floor * .5))
            value = floor;
        else
            value = floor + 1;
        if (negative)
            value = -value;

        if (Is_Rational(num))
            return Double_To_Integer(value);
        else
            return Make_Real(value);
    }
}

Cell p_exp(Cell num) {
    CHECK_N_TYPE(num, Number, "exp", 1);
    return Make_Real(std::exp(Number_To_Double(num)));
}
Cell p_log(Cell num) {
    CHECK_N_TYPE(num, Number, "log", 1);
    double val = Number_To_Double(num);
    S_CHECK(val > 0, "invalid value for log");
    return Make_Real(std::log(val));
}
Cell p_sqrt(Cell num) {
    CHECK_N_TYPE(num, Number, "sqrt", 1);
    double val = Number_To_Double(num);
    S_CHECK(val >= 0, "invalid value for sqrt");
    val = std::sqrt(val);
    if (Double_Is_Int(val))
        return Double_To_Integer(val);
    else
        return Make_Real(val);
}
Cell p_expt(Cell num, Cell exp) {
    CHECK_TYPE(num, Number, 1, "expt");
    CHECK_TYPE(exp, Number, 2, "expt");
    if (Is_Integer(num) && Is_Fixnum(exp)) {
        int expon = Fixnum_Value(exp);
        bool negative = expon < 0;
        if (negative)
            expon = -expon;
        MCell accum = one_cell, base = num;
        for (size_t i = expon; i != 0; --i)
            accum = Number_Multiply(accum, base);
        if (negative)
            accum = Make_Simplified_Rational(one_cell, accum);
        return accum;
    } else {
        double base = Number_To_Double(num), expon = Number_To_Double(exp);
        S_CHECK(base != 0 || expon >= 0, "raising 0 to a negative power");
        return Make_Real(std::pow(base, expon));
    }
}

Cell p_sin(Cell num) {
    CHECK_N_TYPE(num, Number, "sin", 1);
    return Make_Real(std::sin(Number_To_Double(num)));
}
Cell p_cos(Cell num) {
    CHECK_N_TYPE(num, Number, "cos", 1);
    return Make_Real(std::cos(Number_To_Double(num)));
}
Cell p_tan(Cell num) {
    CHECK_N_TYPE(num, Number, "tan", 1);
    return Make_Real(std::tan(Number_To_Double(num)));
}
Cell p_asin(Cell num) {
    CHECK_N_TYPE(num, Number, "asin", 1);
    double val = Number_To_Double(num);
    S_CHECK(val <= 1.0 && val >= -1.0, "invalid value for asin");
    return Make_Real(std::asin(val));
}
Cell p_acos(Cell num) {
    CHECK_N_TYPE(num, Number, "acos", 1);
    double val = Number_To_Double(num);
    S_CHECK(val <= 1.0 && val >= -1.0, "invalid value for acos");
    return Make_Real(std::acos(val));
}
Cell p_atan(Cell num, Cell opt) {
    const double pi = 3.14159265358979323846;
    CHECK_TYPE(num, Number, 1, "atan");
    if (OPTIONAL(opt, "atan")) {
        CHECK_TYPE(Car(opt), Number, 2, "atan");
        double y = Number_To_Double(num), x = Number_To_Double(Car(opt));
        double retval;
        if (x == 0) {
            if (y > 0)
                retval = pi * .5;
            else if (y < 0)
                retval = pi * -.5;
            else
                throw Scheme_Error(
                    "atan is undefined when both arguments are 0");
        } else {
            retval = std::atan(y / x);
            if (x <= 0)
                retval += pi;
        }
        return Make_Real(retval);
    } else {
        return Make_Real(std::atan(Number_To_Double(num)));
    }
}

Cell p_exact_to_inexact(Cell num) {
    CHECK_N_TYPE(num, Number, "exact->inexact", 1);
    return Make_Real(Number_To_Double(num));
}
Cell p_inexact_to_exact(Cell num) {
    const size_t mult_factor = 232792560; // divisible by everything under 22
    CHECK_N_TYPE(num, Number, "inexact->exact", 1);
    if (Is_Real(num)) {
        Cell numerator = Double_To_Integer(Real_Value(num) * mult_factor);
        return Make_Simplified_Rational(numerator, Make_Fixnum(mult_factor));
    } else {
        return num;
    }
}

// symbols

Cell p_symbolp(Cell cell) { return Make_Bool(Is_Symbol(cell)); }
Cell p_symbol_to_string(Cell cell) {
    CHECK_N_TYPE(cell, Symbol, "symbol->string", 1);
    return Make_String(Symbol_Name(cell));
}
Cell p_string_to_symbol(Cell cell) {
    CHECK_N_TYPE(cell, String, "string->symbol", 1);
    return Make_Symbol(String_Value(cell));
}
Cell p_symbol_to_integer(Cell cell) {
    CHECK_N_TYPE(cell, Symbol, "symbol->integer", 1);
    return Make_Fixnum(Symbol_Value(cell));
}
Cell p_integer_to_symbol(Cell cell) {
    CHECK_N_TYPE(cell, Fixnum, "integer->symbol", 1);
    return Make_Symbol(Fixnum_Value(cell));
}

// characters

Cell p_charp(Cell cell) { return Make_Bool(Is_Character(cell)); }

Cell p_char_equal(Cell one, Cell two) {
    CHECK_TYPE(one, Character, 1, "char=?");
    CHECK_TYPE(two, Character, 2, "char=?");
    return Make_Bool(Character_Value(one) == Character_Value(two));
}
Cell p_char_less(Cell one, Cell two) {
    CHECK_TYPE(one, Character, 1, "char<?");
    CHECK_TYPE(two, Character, 2, "char<?");
    return Make_Bool(Character_Value(one) < Character_Value(two));
}
Cell p_char_less_equal(Cell one, Cell two) {
    CHECK_TYPE(one, Character, 1, "char<=?");
    CHECK_TYPE(two, Character, 2, "char<=?");
    return Make_Bool(Character_Value(one) <= Character_Value(two));
}
Cell p_char_greater(Cell one, Cell two) {
    CHECK_TYPE(one, Character, 1, "char>?");
    CHECK_TYPE(two, Character, 2, "char>?");
    return Make_Bool(Character_Value(one) > Character_Value(two));
}
Cell p_char_greater_equal(Cell one, Cell two) {
    CHECK_TYPE(one, Character, 1, "char>=?");
    CHECK_TYPE(two, Character, 2, "char>=?");
    return Make_Bool(Character_Value(one) >= Character_Value(two));
}

Cell p_char_to_integer(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char->integer", 1);
    return Make_Fixnum(Character_Value(cell));
}
Cell p_integer_to_char(Cell cell) {
    CHECK_N_TYPE(cell, Fixnum, "integer->char", 1);
    return Make_Character(Fixnum_Value(cell));
}

Cell p_char_upcase(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char-upcase", 1);
    return Make_Character(std::toupper(Character_Value(cell)));
}
Cell p_char_downcase(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char-downcase", 1);
    return Make_Character(std::tolower(Character_Value(cell)));
}

Cell p_char_alphabetic(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char-alphabetic", 1);
    return Make_Bool(std::isalpha(Character_Value(cell)));
}
Cell p_char_numeric(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char-numeric", 1);
    return Make_Bool(std::isdigit(Character_Value(cell)));
}
Cell p_char_whitespace(Cell cell) {
    CHECK_N_TYPE(cell, Character, "char-whitespace", 1);
    return Make_Bool(Is_Whitespace(Character_Value(cell)));
}

// strings

Cell p_stringp(Cell cell) { return Make_Bool(Is_String(cell)); }

Cell p_make_string(Cell length, Cell fill) {
    CHECK_TYPE(length, Fixnum, 1, "make-string");
    S_CHECK(Fixnum_Value(length) >= 0,
            "first argument to make-string must be non-negative");
    Cell fill_char = Make_Character(' ');
    if (OPTIONAL(fill, "make-string")) {
        CHECK_TYPE(Car(fill), Character, 2, "make-string");
        fill_char = Car(fill);
    }
    return Make_String(Fixnum_Value(length), Character_Value(fill_char));
}
Cell p_string_length(Cell str) {
    CHECK_N_TYPE(str, String, "string-length", 1);
    return Make_Fixnum(String_Size(str));
}
Cell p_string_ref(Cell str, Cell index) {
    CHECK_TYPE(str, String, 1, "string-ref");
    CHECK_TYPE(index, Fixnum, 2, "string-ref");
    int i = Fixnum_Value(index);
    S_CHECK(i >= 0 && static_cast<size_t>(i) < String_Size(str),
            "index out of range in string-ref");
    return Make_Character(String_Ref(str, i));
}
Cell p_string_set(Cell str, Cell index, Cell value) {
    CHECK_TYPE(str, String, 1, "string-set");
    CHECK_TYPE(index, Fixnum, 2, "string-set");
    CHECK_TYPE(value, Character, 3, "string-set");
    int i = Fixnum_Value(index);
    S_CHECK(i >= 0 && static_cast<size_t>(i) < String_Size(str),
            "index out of range in string-set!");
    String_Ref(str, i) = Character_Value(value);
    return void_cell;
}

// vector

Cell p_vectorp(Cell cell) { return Make_Bool(Is_Vector(cell)); }

Cell p_make_vector(Cell length, Cell fill) {
    CHECK_TYPE(length, Fixnum, 1, "make-vector");
    S_CHECK(Fixnum_Value(length) >= 0,
            "first argument to make-vector must be non-negative");
    Cell fill_value = false_cell;
    if (OPTIONAL(fill, "make-vector"))
        fill_value = Car(fill);
    return Make_Vector(Fixnum_Value(length), fill_value);
}
Cell p_vector_length(Cell vector) {
    CHECK_N_TYPE(vector, Vector, "vector-length", 1);
    return Make_Fixnum(Vector_Size(vector));
}
Cell p_vector_ref(Cell vector, Cell index) {
    CHECK_TYPE(vector, Vector, 1, "vector-ref");
    CHECK_TYPE(index, Fixnum, 2, "vector-ref");
    int i = Fixnum_Value(index);
    S_CHECK(i >= 0 && static_cast<size_t>(i) < Vector_Size(vector),
            "index out of range in vector-ref");
    return Vector_Ref(vector, i);
}
Cell p_vector_set(Cell vector, Cell index, Cell value) {
    CHECK_TYPE(vector, Vector, 1, "vector-set!");
    CHECK_TYPE(index, Fixnum, 2, "vector-set!");
    int i = Fixnum_Value(index);
    S_CHECK(i >= 0 && static_cast<size_t>(i) < Vector_Size(vector),
            "index out of range in vector-set!");
    Vector_Ref(vector, i) = value;
    return void_cell;
}

// i/o

Cell p_input_portp(Cell cell) { return Make_Bool(Is_Inport(cell)); }
Cell p_output_portp(Cell cell) { return Make_Bool(Is_Outport(cell)); }

// Common pattern of functions taking an optional output port arg
Cell GET_OUTPORT(int n, Cell port, const char *function_name) {
    Cell output = Interpreter::Output();
    if (OPTIONAL(port, function_name)) {
        CHECK_TYPE(Car(port), Outport, n, function_name);
        output = Car(port);
    }
    return output;
}

Cell p_write(Cell value, Cell port) {
    Write(value, Outport_Stream(GET_OUTPORT(2, port, "write")));
    return void_cell;
}
Cell p_write_char(Cell cell, Cell port) {
    CHECK_TYPE(cell, Character, 1, "write-char");
    Write(cell, Outport_Stream(GET_OUTPORT(2, port, "write-char")), true);
    return void_cell;
}
Cell p_display(Cell value, Cell port) {
    Write(value, Outport_Stream(GET_OUTPORT(2, port, "display")), true);
    return void_cell;
}
Cell p_newline(Cell port) {
    Outport_Stream(GET_OUTPORT(1, port, "newline")) << std::endl;
    return void_cell;
}
Cell p_flush_output(Cell port) {
    Outport_Stream(GET_OUTPORT(1, port, "flush-output")).flush();
    return void_cell;
}

Cell GET_INPORT(Cell port, const char *function_name) {
    Cell input = Interpreter::Input();
    if (OPTIONAL(port, function_name)) {
        CHECK_TYPE(Car(port), Inport, 1, function_name);
        input = Car(port);
    }
    return input;
}

Cell p_read(Cell port) {
    return Read(GET_INPORT(port, "read"));
}
Cell p_read_char(Cell port) {
    return Inport_Read_Char(GET_INPORT(port, "read-char"));
}
Cell p_peek_char(Cell port) {
    return Inport_Peek_Char(GET_INPORT(port, "peek-char"));
}
Cell p_eof_objectp(Cell cell) { return Make_Bool(cell == eof_cell); }
Cell p_char_readyp(Cell port) {
    return Make_Bool(Inport_Ready(GET_INPORT(port, "char-ready?")));
}

Cell p_open_input_file(Cell filename) {
    CHECK_N_TYPE(filename, String, "open-input-file", 1);
    return Make_Inport(filename);
}
Cell p_reopen_input_file(Cell port) {
    CHECK_N_TYPE(port, Inport, "reopen-input-file", 1);
    Reopen_Inport(port);
    return void_cell;
}
Cell p_open_output_file(Cell filename) {
    CHECK_N_TYPE(filename, String, "open-output-file", 1);
    return Make_Outport(filename);
}
Cell p_reopen_output_file(Cell port) {
    CHECK_N_TYPE(port, Outport, "reopen-output-file", 1);
    Reopen_Outport(port);
    return void_cell;
}
Cell p_close_input_port(Cell port) {
    CHECK_N_TYPE(port, Inport, "close-input-port", 1);
    Close_Inport(port);
    return void_cell;
}
Cell p_close_output_port(Cell port) {
    CHECK_N_TYPE(port, Outport, "close-output-port", 1);
    Close_Outport(port);
    return void_cell;
}
Cell p_current_input_port() { return Interpreter::Input(); }
Cell p_current_output_port() { return Interpreter::Output(); }

Cell p_file_existsp(Cell filename) {
    CHECK_N_TYPE(filename, String, "file-exists?", 1);
    std::ifstream test(String_Value(filename).c_str());
    return Make_Bool(!test.fail());
}
Cell p_input_port_line(Cell port) {
    CHECK_N_TYPE(port, Inport, "input-port-line", 1);
    return Make_Integer(Inport_Line(port));
}

// procedure

Cell p_procedurep(Cell cell) {
    return Make_Bool(Is_Closure(cell) || Is_Primitive(cell));
}

// non-standard functions

// used to make closures using instruction vectors
Cell p_make_closure(Cell code, Cell num_args) {
    CHECK_N_TYPE(code, Vector, "make-closure", 1);
    CHECK_N_TYPE(num_args, Fixnum, "make-closure", 2);
    return Make_Closure(code, null_cell, num_args, false_cell);
}
// raise an error
Cell p_raise(Cell message) {
    CHECK_N_TYPE(message, String, "raise", 1);
    throw Scheme_Error(String_Value(message));
}
Cell p_collect_garbage() {
    Memory_Manager::mp_->Collect_Garbage();
    return void_cell;
}
Cell p_definedp(Cell symbol, Cell name_space) {
    CHECK_TYPE(symbol, Symbol, 1, "defined?");
    CHECK_TYPE(name_space, Namespace, 2, "defined?");
    return Make_Bool(Get_Value(name_space, symbol) != invalid_cell);
}
Cell p_environmentp(Cell cell) { return Make_Bool(Is_Namespace(cell)); }

Cell p_random(Cell number) {
    int max = 100;
    if (OPTIONAL(number, "random")) {
        CHECK_TYPE(Car(number), Fixnum, 1, "random");
        max = Fixnum_Value(Car(number));
        S_CHECK(max > 0, "argument 1 to random must be positive");
    }
    return Make_Fixnum(std::abs(rand() % max));
}

Cell p_seed_random(Cell number) {
    CHECK_N_TYPE(number, Fixnum, "seed-random", 1);
    srand(Fixnum_Value(number));
    return void_cell;
}
Cell p_current_time() { return Make_Integer(time(nullptr)); }

// convert any object to string
Cell p_object_to_string(Cell cell, Cell opt_display) {
    bool display = false;
    if (OPTIONAL(opt_display, "object->string"))
        display = Car(opt_display) != false_cell;
    std::string str = Cell_To_String(cell, display);
    return Make_String(str);
}

// environments

Cell p_null_environment(Cell version) {
    CHECK_N_TYPE(version, Fixnum, "null-environment", 1);
    S_CHECK(Fixnum_Value(version) == 5,
            "argument 1 to null-environment must be 5");
    MCell retval = Make_Namespace(Interpreter::NullEnv(), default_workspace_size);
    Bootstrap_Namespace(retval);
    return retval;
}

Cell p_scheme_report_environment(Cell version) {
    CHECK_N_TYPE(version, Fixnum, "scheme-report-environment", 1);
    S_CHECK(Fixnum_Value(version) == 5,
            "argument 1 to scheme-report-environment must be 5");
    MCell retval = Make_Namespace(Interpreter::ReportEnv(), default_workspace_size);
    Bootstrap_Namespace(retval);
    return retval;
}

// used by eval, creates a closure of 0 arguments containing the code
// from expression and optionally a name
Cell p_compile(Cell expression, Cell name_space, Cell opt_name) {
    CHECK_TYPE(name_space, Namespace, 2, "compile");
    Cell name = false_cell;
    if (OPTIONAL(opt_name, "compile")) {
        CHECK_TYPE(Car(opt_name), Symbol, 3, "compile");
        name = Car(opt_name);
    }
    return Make_Closure(Compile(expression, name_space), null_cell, zero_cell,
                        name);
}

#undef CHECK_N_TYPE

#define PRIMITIVE(name, function)                                              \
    Define_Symbol(name_space, name, Make_Primitive(function, name))
#define PRIMITIVE_V(name, function)                                            \
    Define_Symbol(name_space, name, Make_Primitive(function, name, true))

void Initialize_Primitives(const MCell &name_space) {
    PRIMITIVE("eq?", p_eqp);
    PRIMITIVE("eqv?", p_eqvp);
    PRIMITIVE("equal?", p_equalp);

    PRIMITIVE("car", p_car);
    PRIMITIVE("cdr", p_cdr);
    PRIMITIVE("cons", p_cons);
    PRIMITIVE("set-car!", p_set_car);
    PRIMITIVE("set-cdr!", p_set_cdr);
    PRIMITIVE("null?", p_nullp);
    PRIMITIVE("pair?", p_pairp);
    PRIMITIVE("list?", p_listp);

    PRIMITIVE("number?", p_numberp);
    PRIMITIVE("integer?", p_integerp);
    PRIMITIVE("rational?", p_rationalp);
    PRIMITIVE("real?", p_realp);
    PRIMITIVE("complex?", p_realp);
    PRIMITIVE("exact?", p_exactp);
    PRIMITIVE("inexact?", p_inexactp);

    // two-arg versions. the arbitrary-arg versions are implemented in
    // init.scm
    PRIMITIVE("_=", p_equals);
    PRIMITIVE("_<", p_less);
    PRIMITIVE("_<=", p_less_equal);
    PRIMITIVE("_>", p_greater);
    PRIMITIVE("_>=", p_greater_equal);

    PRIMITIVE("_+", p_plus);
    PRIMITIVE("_-", p_minus);
    PRIMITIVE("_*", p_times);
    PRIMITIVE("_/", p_divide);

    PRIMITIVE("quotient", p_quotient);
    PRIMITIVE("remainder", p_remainder);
    PRIMITIVE("modulo", p_modulo);

    PRIMITIVE_V("number->string", p_number_to_string);
    PRIMITIVE_V("string->number", p_string_to_number);

    PRIMITIVE("numerator", p_numerator);
    PRIMITIVE("denominator", p_denominator);

    PRIMITIVE("ceiling", p_ceiling);
    PRIMITIVE("floor", p_floor);
    PRIMITIVE("truncate", p_truncate);
    PRIMITIVE("round", p_round);

    PRIMITIVE("exp", p_exp);
    PRIMITIVE("log", p_log);
    PRIMITIVE("sqrt", p_sqrt);
    PRIMITIVE("expt", p_expt);

    PRIMITIVE("sin", p_sin);
    PRIMITIVE("cos", p_cos);
    PRIMITIVE("tan", p_tan);
    PRIMITIVE("asin", p_asin);
    PRIMITIVE("acos", p_acos);
    PRIMITIVE_V("atan", p_atan);

    PRIMITIVE("exact->inexact", p_exact_to_inexact);
    PRIMITIVE("inexact->exact", p_inexact_to_exact);

    PRIMITIVE("symbol?", p_symbolp);
    PRIMITIVE("string->symbol", p_string_to_symbol);
    PRIMITIVE("symbol->string", p_symbol_to_string);
    PRIMITIVE("symbol->integer", p_symbol_to_integer);
    PRIMITIVE("integer->symbol", p_integer_to_symbol);

    PRIMITIVE("char?", p_charp);
    PRIMITIVE("char_=?", p_char_equal);
    PRIMITIVE("char_<?", p_char_less);
    PRIMITIVE("char_<=?", p_char_less_equal);
    PRIMITIVE("char_>?", p_char_greater);
    PRIMITIVE("char_>=?", p_char_greater_equal);

    PRIMITIVE("char->integer", p_char_to_integer);
    PRIMITIVE("integer->char", p_integer_to_char);
    PRIMITIVE("char-upcase", p_char_upcase);
    PRIMITIVE("char-downcase", p_char_downcase);
    PRIMITIVE("char-alphabetic?", p_char_alphabetic);
    PRIMITIVE("char-numeric?", p_char_numeric);
    PRIMITIVE("char-whitespace?", p_char_whitespace);

    PRIMITIVE("string?", p_stringp);
    PRIMITIVE_V("make-string", p_make_string);
    PRIMITIVE("string-length", p_string_length);
    PRIMITIVE("string-ref", p_string_ref);
    PRIMITIVE("string-set!", p_string_set);

    PRIMITIVE("vector?", p_vectorp);
    PRIMITIVE_V("make-vector", p_make_vector);
    PRIMITIVE("vector-length", p_vector_length);
    PRIMITIVE("vector-ref", p_vector_ref);
    PRIMITIVE("vector-set!", p_vector_set);

    PRIMITIVE("input-port?", p_input_portp);
    PRIMITIVE("output-port?", p_output_portp);

    PRIMITIVE_V("write", p_write);
    PRIMITIVE_V("write-char", p_write_char);
    PRIMITIVE_V("display", p_display);
    PRIMITIVE_V("newline", p_newline);

    PRIMITIVE_V("read", p_read);
    PRIMITIVE_V("read-char", p_read_char);
    PRIMITIVE_V("char-ready?", p_char_readyp);
    PRIMITIVE_V("peek-char", p_peek_char);
    PRIMITIVE("eof-object?", p_eof_objectp);
    PRIMITIVE_V("flush-output", p_flush_output);

    PRIMITIVE("open-input-file", p_open_input_file);
    PRIMITIVE("open-output-file", p_open_output_file);
    PRIMITIVE("reopen-input-file", p_reopen_input_file);
    PRIMITIVE("reopen-output-file", p_reopen_output_file);
    PRIMITIVE("close-input-port", p_close_input_port);
    PRIMITIVE("close-output-port", p_close_output_port);
    PRIMITIVE("current-input-port", p_current_input_port);
    PRIMITIVE("current-output-port", p_current_output_port);
    PRIMITIVE("file-exists?", p_file_existsp);
    PRIMITIVE("input-port-line", p_input_port_line);

    PRIMITIVE("procedure?", p_procedurep);
    PRIMITIVE("scheme-report-environment", p_scheme_report_environment);
    PRIMITIVE("null-environment", p_null_environment);
    PRIMITIVE_V("compile", p_compile);

    PRIMITIVE("make-closure", p_make_closure);
    PRIMITIVE("raise", p_raise);
    PRIMITIVE("collect-garbage", p_collect_garbage);
    PRIMITIVE("defined?", p_definedp);
    PRIMITIVE("environment?", p_environmentp);

    PRIMITIVE_V("random", p_random);
    PRIMITIVE("seed-random", p_seed_random);
    PRIMITIVE("current-time", p_current_time);
    PRIMITIVE_V("object->string", p_object_to_string);
}

#undef PRIMITIVE
#undef PRIMITIVE_V

// Do the same thing for each of the 9 types of primitive functions.
// It is not pretty but it is convenient for client code.
#define WRITE_FUNCTION(n)                                                      \
    Cell Make_Primitive(Primitive_Function_##n function,                       \
                        const std::string &name, bool var_arg) {               \
        Cell new_prim = Allocate_Cell<Primitive_Data>(primitive_type, (std::byte) 0); \
        Primitive_Data &data = Extract<Primitive_Data>(new_prim);              \
        data.function = reinterpret_cast<Primitive_Function_0>(function);      \
        data.num_args = n;                                                     \
        if (var_arg)                                                           \
            data.num_args = -data.num_args;                                    \
        data.name = Make_Symbol(name);                                         \
        return new_prim;                                                       \
    }

WRITE_FUNCTION(0);
WRITE_FUNCTION(1);
WRITE_FUNCTION(2);
WRITE_FUNCTION(3);
WRITE_FUNCTION(4);
WRITE_FUNCTION(5);
WRITE_FUNCTION(6);
WRITE_FUNCTION(7);
WRITE_FUNCTION(8);

#undef WRITE_FUNCTION




// ,RENAMED SYMBOLS

// Not to be confused with temp names. These are what macro-expansion
// makes of the symbols it finds in its template (the ones that are
// not expanded). They only live inside the expander - after the macro
// gets expanded the expander converts these to either temp names or
// normal symbols (based on whether they refer to a top level variable
// or a local one. They can nest (in case of a macro expanding to a
// macro definition).

Cell Make_Renamed_Symbol(const MCell &old_name, const MCell &macro,
                         const MCell &unique) {
    Cell new_cell = Allocate_Cell<Renamed_Symbol_Data>(renamed_symbol_type);
    Renamed_Symbol_Data &data = Extract<Renamed_Symbol_Data>(new_cell);
    data.old_name = old_name;
    data.macro = macro;
    data.unique_value = unique;
    return new_cell;
}

void Write_Renamed_Symbol(Cell cell, std::ostream &str, bool display) {
    str << "#<renamed:" << (unsigned)Renamed_Symbol_Old_Name(cell) << '>';
}

Cell Extract_Symbol(Cell identifier) {
    while (Is_Renamed_Symbol(identifier))
        identifier = Renamed_Symbol_Old_Name(identifier);
    return identifier;
}

// Turn renamed symbols back into plain symbols inside an expression.
// Needed because the macro expander does not know about quoting and
// quasiquoting, and renames symbols inside quoted expressions. This
// is used to flatten those expressions out again. The function makes
// some attempt to not create new pairs and vectors unless necessary.
Cell Unrename_Expression(const MCell &expression) {
    if (Is_Pair(expression)) {
        MCell car = Car(expression), cdr = Cdr(expression);
        car = Unrename_Expression(car);
        cdr = Unrename_Expression(cdr);
        if (car == Car(expression) && cdr == Cdr(expression))
            return expression;
        else
            return Cons(car, cdr);
    } else if (Is_Vector(expression)) {
        MStack new_values;
        bool changed = false;
        size_t size = Vector_Size(expression);
        for (size_t i = 0; i != size; ++i) {
            new_values.Push(Unrename_Expression(Vector_Ref(expression, i)));
            if (new_values.Back() != Vector_Ref(expression, i))
                changed = true;
        }
        if (changed)
            return Make_Vector(new_values);
        else
            return expression;
    } else if (Is_Renamed_Symbol(expression)) {
        return Extract_Symbol(expression);
    } else {
        return expression;
    }
}

#ifdef WITH_DESTRUCTORS
// Convenience function for defining primitives.
template <typename Function_Type>
inline void Define_Primitive(const std::string &name, Function_Type function,
                             bool var_arg) {
    Define_Symbol(Interpreter::WorkEnv(), name, Make_Primitive(function, name, var_arg));
}
#endif
} // namespace uls
