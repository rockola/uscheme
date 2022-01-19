#include "vector.hpp"
#include "bignum.hpp"
#include "primitive.hpp"
#include "list.hpp"

namespace uls {

// ,VECTOR

Cell Make_Vector(size_t size, Cell fill) {
    Cell new_vector = Allocate(sizeof(Cell) * (size + 1), vector_type);
    Vector_Data &data = Extract<Vector_Data>(new_vector);
    data.size = Make_Fixnum(size);
    for (size_t i = 0; i < size; ++i)
        data.data[i] = fill;
    return new_vector;
}

Cell Make_Vector(const MStack &stack) {
    Cell new_vector = Make_Vector(stack.Size());
    for (size_t i = 0; i != stack.Size(); ++i)
        Vector_Ref(new_vector, i) = stack[i];
    return new_vector;
}

Cell Make_Vector_From_List(Cell list) {
    S_ASSERT(Is_Pair(list));
    size_t size = List_Length(list);
    MCell lst = list;
    Cell new_vec = Make_Vector(size);
    for (size_t i = 0; i != size; ++i, lst = Cdr(lst))
        Vector_Ref(new_vec, i) = Car(lst);
    return new_vec;
}

Cell Copy_Vector(Cell vec) {
    S_ASSERT(Is_Vector(vec));
    size_t size = Vector_Size(vec);
    MCell old_vec = vec;
    Cell new_vec = Make_Vector(size);
    for (size_t i = 0; i != size; ++i)
        Vector_Ref(new_vec, i) = Vector_Ref(vec, i);
    return new_vec;
}

}
