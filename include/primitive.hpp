#pragma once

#include "bignum.hpp"
#include "cell.hpp"
#include "mcell.hpp"
#include "error.hpp"
#include "symbol.hpp"

namespace uls {

struct Pair {
    Cell car, cdr;
};

inline bool Is_Pair(Cell cell) {
    return Is_Compound(cell) && Get_Type(cell) == pair_type;
}

bool Equal(Cell one, Cell two);

// Car and Cdr are used both for getting and setting values.
inline Cell &Car(const Cell cell) {
    S_ASSERT(Is_Pair(cell));
    return Extract<Pair>(cell).car;
}
inline Cell &Cdr(const Cell cell) {
    S_ASSERT(Is_Pair(cell));
    return Extract<Pair>(cell).cdr;
}

// A number of cxr variants.
inline Cell &Caar(const Cell cell) { return Car(Car(cell)); }
inline Cell &Cdar(const Cell cell) { return Cdr(Car(cell)); }
inline Cell &Cadr(const Cell cell) { return Car(Cdr(cell)); }
inline Cell &Cddr(const Cell cell) { return Cdr(Cdr(cell)); }
inline Cell &Caddr(const Cell cell) { return Car(Cdr(Cdr(cell))); }
inline Cell &Cdaar(const Cell cell) { return Cdr(Car(Car(cell))); }
inline Cell &Cadar(const Cell cell) { return Car(Cdr(Car(cell))); }
inline Cell &Cddar(const Cell cell) { return Cdr(Cdr(Car(cell))); }
inline Cell &Caadr(const Cell cell) { return Car(Car(Cdr(cell))); }
inline Cell &Caaar(const Cell cell) { return Car(Car(Car(cell))); }

//

inline size_t Vector_Size(Cell cell) {
    S_ASSERT(Is_Vector(cell));
    return Fixnum_Value(Extract<Vector_Data>(cell).size);
}

struct Renamed_Symbol_Data {
    Cell old_name, macro, unique_value;
};

inline Cell Renamed_Symbol_Old_Name(Cell cell) {
    S_ASSERT(Is_Renamed_Symbol(cell));
    return Extract<Renamed_Symbol_Data>(cell).old_name;
}
inline Cell Renamed_Symbol_Macro(Cell cell) {
    S_ASSERT(Is_Renamed_Symbol(cell));
    return Extract<Renamed_Symbol_Data>(cell).macro;
}

inline Cell Cons(const MCell &car, const MCell &cdr) {
    Cell retval = Allocate_Cell<Pair>(pair_type);
    Car(retval) = car;
    Cdr(retval) = cdr;
    return retval;
}

// Only use this when car and cdr are NOT compounds
inline Cell XCons(Cell car, Cell cdr) {
    Cell retval = Allocate_Cell<Pair>(pair_type);
    Car(retval) = car;
    Cdr(retval) = cdr;
    return retval;
}

// Conses null onto a cell
inline Cell Cons_Null(const MCell &car) {
    Cell retval = Allocate_Cell<Pair>(pair_type);
    Car(retval) = car;
    Cdr(retval) = null_cell;
    return retval;
}

void Initialize_Primitives(const MCell &name_space);

// Easy way of building a list front to end.
class List_Builder {
  public:
    void Add_Element(const MCell &cell) {
        if (_start == null_cell) {
            _start = Cons(cell, _start);
            _tail = _start;
        } else {
            S_ASSERT(_tail != null_cell);
            Cdr(_tail) = Cons_Null(cell);
            _tail = Cdr(_tail);
        }
    }
    void Add_End(Cell cell) {
        if (_start == null_cell) {
            _start = cell;
        } else {
            S_ASSERT(_tail != null_cell);
            Cdr(_tail) = cell;
        }
        _tail = null_cell;
    }
    const MCell &List() { return _start; }

  private:
    MCell _start, _tail;
};

// ,SYMBOL
// Symbol cells, see symbol.hpp and symbol.cpp for the implementation
// of the symbol table.

inline Cell Make_Symbol(Symbol symbol) {
    return Encode_Fourbit(symbol, symbol_pattern);
}
inline Cell Make_Symbol(const std::string &name) {
    return Make_Symbol(Get_Symbol(name));
}
inline Symbol Symbol_Value(Cell cell) { return Extract_Fourbit(cell); }
inline const std::string &Symbol_Name(Cell cell) {
    return Get_Symbol_Name(Symbol_Value(cell));
}

// ,PRIMITIVE

// Primitives contain a function pointer pointing to a function of one
// of the 9 types shown below, a number of arguments and a name. This
// looks very ugly but the advantage is that you can easily define
// primitives with up to 8 arguments. The Make_Primitive will make a
// primitive with the correct number of arguments based on the type of
// function you pass it. If var_arg is true the last argument will
// behave like z in (lambda (x y . z) ....)
typedef Cell (*Primitive_Function_0)();
typedef Cell (*Primitive_Function_1)(Cell one);
typedef Cell (*Primitive_Function_2)(Cell one, Cell two);
typedef Cell (*Primitive_Function_3)(Cell one, Cell two, Cell three);
typedef Cell (*Primitive_Function_4)(Cell one, Cell two, Cell three, Cell four);
typedef Cell (*Primitive_Function_5)(Cell one, Cell two, Cell three, Cell four,
                                     Cell five);
typedef Cell (*Primitive_Function_6)(Cell one, Cell two, Cell three, Cell four,
                                     Cell five, Cell six);
typedef Cell (*Primitive_Function_7)(Cell one, Cell two, Cell three, Cell four,
                                     Cell five, Cell six, Cell seven);
typedef Cell (*Primitive_Function_8)(Cell one, Cell two, Cell three, Cell four,
                                     Cell five, Cell six, Cell seven,
                                     Cell eight);
Cell Make_Primitive(Primitive_Function_0 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_1 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_2 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_3 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_4 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_5 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_6 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_7 function, const std::string &name,
                    bool var_arg = false);
Cell Make_Primitive(Primitive_Function_8 function, const std::string &name,
                    bool var_arg = false);

// The function is stored s a Primitive_Function_0 and cast to the
// proper type (based on num_args) when it is called.
struct Primitive_Data {
    Primitive_Function_0 function;
    int num_args;
    Cell name;
};

// Do the same thing for each of the 9 types of primitive functions.
// It is not pretty but it is convenient for client code.
#define WRITE_FUNCTION(n)                                                      \
    Cell Make_Primitive(Primitive_Function_##n function,                       \
                        const std::string &name, bool var_arg);

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

inline int Primitive_Num_Args(Cell cell) {
    return Extract<Primitive_Data>(cell).num_args;
}
inline Primitive_Function_0 Primitive_Function(Cell cell) {
    return Extract<Primitive_Data>(cell).function;
}
inline Cell Primitive_Name(Cell cell) {
    return Extract<Primitive_Data>(cell).name;
}



// Ugly, repetetive Call_Primitive functions. This is where C++'s
// static function/type system shines (not). If you know any kind of
// template magic that can make this more elegant, tell me!
typedef Cell (*Call_Primitive)(Primitive_Function_0 function, Cell *arguments);

inline Cell Call_Primitive_0(Primitive_Function_0 function, Cell *arguments) {
    return function();
}
inline Cell Call_Primitive_1(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_1>(function)(arguments[0]);
}
inline Cell Call_Primitive_2(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_2>(function)(arguments[0],
                                                            arguments[1]);
}
inline Cell Call_Primitive_3(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_3>(function)(
        arguments[0], arguments[1], arguments[2]);
}
inline Cell Call_Primitive_4(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_4>(function)(
        arguments[0], arguments[1], arguments[2], arguments[3]);
}
inline Cell Call_Primitive_5(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_5>(function)(
        arguments[0], arguments[1], arguments[2], arguments[3], arguments[4]);
}
inline Cell Call_Primitive_6(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_6>(function)(
        arguments[0], arguments[1], arguments[2], arguments[3], arguments[4],
        arguments[5]);
}
inline Cell Call_Primitive_7(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_7>(function)(
        arguments[0], arguments[1], arguments[2], arguments[3], arguments[4],
        arguments[5], arguments[6]);
}
inline Cell Call_Primitive_8(Primitive_Function_0 function, Cell *arguments) {
    return reinterpret_cast<Primitive_Function_8>(function)(
        arguments[0], arguments[1], arguments[2], arguments[3], arguments[4],
        arguments[5], arguments[6], arguments[7]);
}

// Pointer wrappers are a convenient way of wrapping C++ objects in
// scheme cells. Make a class (T) inherit from Pointer_Wrapper<T>, and
// then you can call Init_Type on it somewhere in your initialization
// code, passing it the name of the constructor function for this
// type, the actual function you want to use for constructing such
// object, and optionally a specialized write function. The class
// provides a convenient function for creating the actual scheme cells
// from a pointer - Wrap_Object. The reason you still have to write
// the actual constructor function yourself is that you may want to
// have it take arguments, or behave in some special way.
#ifdef WITH_DESTRUCTORS
// Convenience function for defining primitives.
template <typename Function_Type>
inline void Define_Primitive(const std::string &name, Function_Type function,
                             bool var_arg = false);

template <class T> class Pointer_Wrapper {
  public:
    static Cell_Type type_id;

    static void Default_Write(Cell cell, std::ostream &str, bool display) {
        str << "#<wrapped pointer>";
    }
    static void Destroy(Cell data) { delete Extract<T *>(data); }

    template <typename Function_Type>
    static void Init_Type(std::string constructor_name, Function_Type create,
                          Write_Function write = Default_Write) {
        type_id = Make_Type(write, Destroy);
        Define_Primitive(constructor_name, create);
    }

    static Cell Wrap_Object(T *object) {
        Cell new_cell = Allocate_Cell<T *>(type_id, 0);
        Extract<T *>(new_cell) = object;
        return new_cell;
    }
};

template <class T> Cell_Type Pointer_Wrapper<T>::type_id = 0;
#endif

Cell Make_Renamed_Symbol(const MCell &old_name, const MCell &macro,
                         const MCell &unique);

void Write_Renamed_Symbol(Cell cell, std::ostream &str, bool display);

Cell Extract_Symbol(Cell identifier);

Cell Unrename_Expression(const MCell &expression);

// An identifier (some more misuse of words) means a symbol or a
// renamed symbol.
inline bool Is_Identifier(Cell cell) {
    return Is_Symbol(cell) || Is_Renamed_Symbol(cell);
}


} // namespace uls
