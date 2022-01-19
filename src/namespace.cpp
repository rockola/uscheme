#include "namespace.hpp"
#include "mcell.hpp"
#include "bignum.hpp"
#include "inputsplitter.hpp"
#include "interpreter.hpp"
#include "primitive.hpp"
#include "list.hpp"
#include "macro.hpp"

namespace uls {

// Get a pair (symbol . value) that corresponds to the binding of
// symbol in the namespace. If no such pair is present a new one is
// created with the value of the symbol in underlying namespaces (or
// invalid_cell if no value is found).
Cell Get_Binding(const MCell &name_space, Cell symbol) {
    size_t hashed = Hash_Symbol(symbol, name_space);
    for (Cell element = Namespace_Ref(name_space, hashed); element != null_cell;
         element = Cdr(element)) {
        if (Caar(element) == symbol)
            return Car(element);
    }
    MCell new_binding =
        Cons(symbol, Get_Value(Namespace_Parent(name_space), symbol));
    MCell old_list = Namespace_Ref(name_space, hashed);
    Namespace_Ref(name_space, hashed) = Cons(new_binding, old_list);
    return new_binding;
}


Cell Make_Namespace(const MCell &parent, size_t size) {
    Cell retval = Allocate(sizeof(Namespace_Data) + sizeof(Cell) * (size - 1),
                           namespace_type);
    Namespace_Data &data = Extract<Namespace_Data>(retval);
    data.parent = parent;
    data.size = Make_Fixnum(size);
    for (size_t i = 0; i != size; ++i)
        data.binding[i] = null_cell;
    return retval;
}

void Bootstrap_Namespace(const MCell &name_space) {
    Eval_String("(define (interaction-environment) (impl:current-env))",
                name_space, false);
    Eval_String("(define (load file) (_load file (interaction-environment)))",
                name_space, false);
}

Cell Namespace_Parent(Cell space) {
    S_ASSERT(Is_Namespace(space));
    return Extract<Namespace_Data>(space).parent;
}
size_t Namespace_Size(Cell space) {
    S_ASSERT(Is_Namespace(space));
    return Fixnum_Value(Extract<Namespace_Data>(space).size);
}
Cell &Namespace_Ref(Cell space, size_t index) {
    S_ASSERT(Is_Namespace(space));
    S_ASSERT(index < Namespace_Size(space));
    return Extract<Namespace_Data>(space).binding[index];
}

size_t Hash_Symbol(Cell symbol, Cell name_space) {
    return Symbol_Value(symbol) % Namespace_Size(name_space);
}


// Define a symbol in a namespace
void Define_Symbol(const MCell &name_space, const std::string &name,
                   const MCell &value) {
    Cell slot = Get_Binding(name_space, Make_Symbol(name));
    Cdr(slot) = value;
}


// Find symbol in vector.
unsigned short Environment_Offset(Cell symbol, Cell env_vector) {
    unsigned short retval = 0;
    size_t size = Vector_Size(env_vector);
    for (size_t i = 0; i != size; ++i) {
        if (Vector_Ref(env_vector, i) == symbol)
            return retval;
        ++retval;
    }
    return not_found;
}

// Get ref from environment.
Environment_Ref Find_In_Environment(Cell symbol, Cell environment) {
    Environment_Ref retval = {0, 0};
    while (environment != null_cell) {
        retval.distance = Environment_Offset(symbol, Car(environment));
        if (retval.distance != not_found)
            return retval;
        environment = Cdr(environment);
        ++retval.depth;
    }
    retval.depth = not_found;
    return retval;
}


// Looks up a binding in a list of lists that represent the current
// environment for the expander. Expanders bind two things - local
// vars get bound to temp names and vars introduced by let-syntax or
// letrec-syntax get bound to macro's. Takes care of renamed symbols
// and their embedded environments. If nothing matches the symbol
// itself is returned.
Cell Resolve_Name(Cell identifier, Cell env, Cell name_space, bool syntax_value,
                  bool one_deep) {
    S_ASSERT(Is_Identifier(identifier));
    Cell id = identifier;

    Cell found = null_cell;
    for (; env != null_cell; env = Cdr(env)) {
        found = Assoc(id, Car(env));
        if (found != null_cell && (syntax_value || !Is_Syntax(Cdr(found)))) {
            found = Cdr(found);
            break;
        }
        if (one_deep)
            break;
    }

    if (found != null_cell) {
        return found;
    } else if (Is_Renamed_Symbol(identifier) && !one_deep) {
        Cell macro = Renamed_Symbol_Macro(identifier);
        return Resolve_Name(Renamed_Symbol_Old_Name(identifier),
                            Macro_Environment(macro), Macro_Namespace(macro),
                            syntax_value);
    } else if (syntax_value) {
        Cell ns_value = Get_Value(name_space, identifier);
        if (Is_Syntax(ns_value))
            return ns_value;
        else
            return identifier;
    } else {
        return identifier;
    }
}

} // namespace uls
