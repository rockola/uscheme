#pragma once

#include "cell.hpp"
#include "mcell.hpp"

namespace uls {

struct Macro_Data {
    Cell special_symbols, rules, name, local_env, name_space;
};

Cell Make_Macro(const MCell &specials, const MCell &rules, Cell name,
                const MCell &local_env, const MCell &name_space);

inline Cell Macro_Name(Cell macro) {
    S_ASSERT(Is_Macro(macro));
    return Extract<Macro_Data>(macro).name;
}
inline Cell Macro_Specials(Cell macro) {
    S_ASSERT(Is_Macro(macro));
    return Extract<Macro_Data>(macro).special_symbols;
}
inline Cell Macro_Rules(Cell macro) {
    S_ASSERT(Is_Macro(macro));
    return Extract<Macro_Data>(macro).rules;
}
inline Cell Macro_Environment(Cell macro) {
    S_ASSERT(Is_Macro(macro));
    return Extract<Macro_Data>(macro).local_env;
}
inline Cell Macro_Namespace(Cell macro) {
    S_ASSERT(Is_Macro(macro));
    return Extract<Macro_Data>(macro).name_space;
}

Cell Expand_Simple_Macro(const MCell &macro, const MCell &args);

// Old-style macros that just call a function to do the expansion.

inline Cell Make_Simple_Macro(const MCell &function) {
    S_CHECK(Is_Closure(function),
            "define-macro requires a closure as it's second argument");
    Cell new_macro = Allocate_Cell<Cell>(simple_macro_type);
    Extract<Cell>(new_macro) = function;
    return new_macro;
}

inline Cell Simple_Macro_Function(Cell macro) {
    S_ASSERT(Is_Simple_Macro(macro));
    return Extract<Cell>(macro);
}

// System to expand macro's. I never expected that ellipses pattern
// language to be so messy until I implemented it.
class Macro_Expander {
  public:
    // External interface. You create a Macro_Expander and then apply it
    // to an expression (a list of arguments to that macro). A
    // macro_expander can be used more than once but the current code
    // (in Expander::Macro_Expand) does not do this.
    Macro_Expander(Cell macro);
    Cell Apply(const MCell &input);

  private:
    // Used to determine whether an expression matches to any
    // non-literals. This is used to figure out when to stop expanding
    // patterns with ellipses behind them.
    enum Match_Status { good, not_bad, bad };
    Match_Status Combine(Match_Status one, Match_Status two);
    Match_Status Contains_Another_Match(MStack &values, Cell pattern);

    // During expansion the matched expressions are stored in two
    // MStacks, one with the names of the variables (_var_names), and a
    // local one with their value trees. These trees get shorter as the
    // expander enters ellipses subtemplates. Var_Number gives the index
    // of a variable, Find_Bound_Vars finds all the meaningful variables
    // in a pattern and adds them to _var_names.
    int Var_Number(Cell var);
    void Find_Bound_Vars(MStack &target, Cell pattern);
    void Build_Subtree_Vector(MStack &target, MStack &source,
                              size_t branch_num);

    // These do the actual matching and expanding. Add_Ellipses just
    // pastes some ellipses after a pattern and is used to expand
    // multiple ellipses in a row.
    Cell Add_Ellipses(const MCell &base, size_t amount);
    Cell Match_Pattern(const MCell &pattern, const MCell &input);
    Cell Fill_In_Template(MStack &var_values, const MCell &tmpl,
                          bool use_ellipses);

    MStack _var_names;
    MCell _macro, _unique_value;
    const MCell dot_dot_dot;
};

// ,EXPANDER

// Before code is compiled it is run through the expander. This has a
// number of effects. The most important effect is that macro's get
// expanded, related to that embedded let-syntax and letrec-syntax
// expressions vanish. To make hygiene workable all local symbols get
// new unique names (temp-names). All listst starting with a symbol
// referring to a special form get their first element dereferenced.
// On top of that a few special forms get transformed. If expressions
// get a #v element added if no 'else expresson' was given. Defines
// that have a list as their first argument get transformed to normal
// defines with a lambda a second argument. Lambda's get a fixnum
// representing the amount of internal defines as a first argument.
// Begins that contain defines get transformed to lambda expressions
// so that remaining begins can be inlined. Quasiquote expressions get
// a vector of expanded unquoted parts as a first argument.
class Expander {
  public:
    Expander(Cell name_space, Cell local_env, size_t cur_name);
    Cell Expand(Cell expression);

  private:
    Cell Expand_Body(Cell expression, bool wrap);
    Cell Expand_Form(Cell syntax, Cell tail, bool define_allowed);
    Cell Macro_Expand(Cell expression);

    bool Look_For_Defines(Cell expression, MStack &results,
                          MStack &defined_symbols);
    Cell Make_Name();

    size_t _cur_name;
    MCell _name_space, _local_env;
};

} // namespace uls
