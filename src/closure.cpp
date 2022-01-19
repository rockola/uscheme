#include "cell.hpp"
#include "closure.hpp"
#include "bignum.hpp"
#include "vector.hpp"

namespace uls {

// ,CLOSURE

Cell Make_Closure(const MCell &code, const MCell &environment, Cell num_args,
                  Cell name) {
    Cell retval = Allocate_Cell<Closure_Data>(closure_type, (std::byte) 6);
    Closure_Data &data = Extract<Closure_Data>(retval);
    data.code = code;
    data.environment = environment;
    data.num_args = Fixnum_Value(num_args);
    data.name = name;
    return retval;
}
// Unfinished closures are the things the compiler generates, the
// environment has to be added to them at run-time. They are simply a
// 3-element vector containing a code vector, a number of arguments
// and a name.
Cell Make_Closure(const MCell &unfinished_closure, const MCell &environment) {
    Cell retval = Allocate_Cell<Closure_Data>(closure_type, (std::byte) 6);
    Closure_Data &data = Extract<Closure_Data>(retval);
    data.code = Vector_Ref(unfinished_closure, 0);
    data.environment = environment;
    data.num_args = Fixnum_Value(Vector_Ref(unfinished_closure, 1));
    data.name = Vector_Ref(unfinished_closure, 2);
    ;
    return retval;
}
Cell Make_Unfinished_Closure(const MCell &code, Cell num_args, Cell name) {
    Cell closure_vector = Make_Vector(3);
    Vector_Ref(closure_vector, 0) = code;
    Vector_Ref(closure_vector, 1) = num_args;
    Vector_Ref(closure_vector, 2) = name;
    return closure_vector;
}

} // namespace uls
