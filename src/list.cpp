#include "list.hpp"
#include "primitive.hpp"

namespace uls {

// The standard assoc
Cell Assoc(Cell needle, Cell list) {
    for (; list != null_cell; list = Cdr(list)) {
        if (Equal(Caar(list), needle))
            return Car(list);
    }
    return null_cell;
}

// This is not entirely the same as the scheme function member - this
// returns a bool instead of a piece of list.
bool Member(Cell value, Cell list) {
    for (; list != null_cell; list = Cdr(list)) {
        S_ASSERT(Is_Pair(list));
        if (Equal(Car(list), value))
            return true;
    }
    return false;
}

// Utility functions
size_t List_Length(Cell list, const char *error) {
    size_t result = 0;
    while (list != null_cell) {
        S_CHECK(Is_Pair(list), error);
        ++result;
        list = Cdr(list);
    }
    return result;
}
bool Is_Proper_List(Cell list) {
    for (; list != null_cell; list = Cdr(list)) {
        if (!Is_Pair(list))
            return false;
    }
    return true;
}

// Append a cell to the end of a list (cell can be a list).
Cell Append_To_List(Cell cell, Cell lst) {
    if (lst == null_cell)
        return cell;

    Cell tmp = lst;
    for (; Cdr(tmp) != null_cell; tmp = Cdr(tmp))
        ;
    Cdr(tmp) = cell;
    return lst;
}

// Insert a matched expression obtained from matching an 'ellipsed'
// subpattern to an expression into a matched expression containing
// the result of earlier matchings of this subpattern.
void Integrate_Tree(Cell element, MCell &tree) {
    for (MCell current = element; current != null_cell;
         current = Cdr(current)) {
        MCell found = Assoc(Caar(current), tree);
        if (found == null_cell) {
            Cdar(current) = Cons_Null(Cdar(current));
            tree = Cons(Car(current), tree);
        } else {
            MCell tmp = Cdar(current);
            tmp = Cons_Null(tmp);
            Cdr(found) = Append_To_List(tmp, Cdr(found));
        }
    }
}

} // namespace uls
