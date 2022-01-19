#include "string.hpp"
#include "symbol.hpp"
#include "primitive.hpp"
#include "list.hpp"
#include "vector.hpp"
#include "form.hpp"
#include "namespace.hpp"
#include "instruction.hpp"
#include "compiler.hpp"
#include "interpreter.hpp"
#include "macro.hpp"

namespace uls {

Cell Expand_Simple_Macro(const MCell &macro, const MCell &args) {
    MCell code = Make_Vector(6);
    Vector_Ref(code, 0) = Make_Instruction(i_literal);
    Vector_Ref(code, 1) = args;
    Vector_Ref(code, 2) = Make_Instruction(i_as_arguments);
    Vector_Ref(code, 3) = Make_Instruction(i_literal);
    Vector_Ref(code, 4) = Simple_Macro_Function(macro);
    Vector_Ref(code, 5) = Make_Instruction(i_tail);
    return Run_Code(code, false);
}

Macro_Expander::Macro_Expander(Cell macro)
    : _macro(macro),
      // to uniquely recognize renamed symbols instantiated by this
      // macro expansion
      _unique_value(Make_String(0)), dot_dot_dot(Make_Symbol("...")) {}

// Find out which variables exist in a pattern as non-literal,
// non-ellipsis variables.
void Macro_Expander::Find_Bound_Vars(MStack &target, Cell pattern) {
    if (Is_Pair(pattern)) {
        Find_Bound_Vars(target, Car(pattern));
        Find_Bound_Vars(target, Cdr(pattern));
    } else if (Is_Vector(pattern)) {
        size_t size = Vector_Size(pattern);
        for (size_t i = 0; i != size; ++i)
            Find_Bound_Vars(target, Vector_Ref(pattern, i));
    } else if (Is_Identifier(pattern) && pattern != dot_dot_dot &&
               !Member(pattern, Macro_Specials(_macro))) {
        target.Push(pattern);
    }
}

// -1 for not found
int Macro_Expander::Var_Number(Cell var) {
    size_t size = _var_names.Size();
    for (size_t i = 0; i != size; ++i) {
        if (Equal(var, _var_names[i]))
            return i;
    }
    return -1;
}

// Extract a matched expression from a pattern and an input
// expression. Will return #f/false_cell if the two did not match, and
// a list of variable - value tree pairs if it does match. A value
// tree contains just a value if the variable appeared without
// ellipses, otherwise it is a list of value trees for each of the
// matched instances. All values are prefixed with an invalid_cell to
// be able to determine the difference between parts of the value tree
// and actual values. And this function is definitely too big, same
// goes for Fill_In_Tree.
Cell Macro_Expander::Match_Pattern(const MCell &pattern, const MCell &input) {
    if (Is_Identifier(pattern)) {
        if (Member(pattern, Macro_Specials(_macro))) { // literal
            if (Equal(Extract_Symbol(input), Extract_Symbol(pattern)))
                return null_cell;
            else
                return false_cell;
        } else { // normal pattern var
            MCell tmp = invalid_cell;
            tmp = Cons(tmp, input);
            tmp = Cons(pattern, tmp);
            return Cons_Null(tmp); // ((pattern #<invalid> . input))
        }
    } else if (Is_Pair(pattern)) {
        if (Is_Pair(Cdr(pattern)) &&
            Cadr(pattern) == dot_dot_dot) { // (pattern ...)
            S_CHECK(Cddr(pattern) == null_cell,
                    "'...' appearing in invalid position in macro pattern");
            MCell result = null_cell;

            // add all bound identifiers in pattern to result
            MStack bound;
            Find_Bound_Vars(bound, Car(pattern));
            for (size_t i = 0; i != bound.Size(); ++i)
                result = Cons(Cons_Null(bound[i]), result);

            // match the elements of input against the pattern
            for (MCell current = input; current != null_cell;
                 current = Cdr(current)) {
                if (!Is_Pair(current)) // ellipses only match proper lists
                    return false_cell;
                Cell matched = Match_Pattern(Car(pattern), Car(current));
                if (matched == false_cell)
                    return false_cell;
                Integrate_Tree(matched, result);
            }

            return result;
        } else if (!Is_Pair(input)) {
            return false_cell;
        } else { // (pattern1 . pattern2)
            MCell car = Car(input);
            car = Match_Pattern(Car(pattern), car);
            if (car == false_cell)
                return false_cell;
            MCell cdr = Cdr(input);
            cdr = Match_Pattern(Cdr(pattern), cdr);
            if (cdr == false_cell)
                return false_cell;
            return Append_To_List(cdr, car);
        }
    } else if (Is_Vector(pattern)) { // #(pattern1 - patternN)
        if (!Is_Vector(input))
            return false_cell;
        size_t pat_size = Vector_Size(pattern), input_size = Vector_Size(input);
        bool ellipses =
            pat_size > 1 && Vector_Ref(pattern, pat_size - 1) == dot_dot_dot;
        if (ellipses)
            pat_size -= 2;
        if ((!ellipses && pat_size != input_size) ||
            (ellipses && input_size < pat_size))
            return false_cell;
        MCell result = null_cell;
        for (size_t i = 0; i != pat_size; ++i)
            result = Append_To_List(
                Match_Pattern(Vector_Ref(pattern, i), Vector_Ref(input, i)),
                result);

        if (ellipses) { // #(pattern1 - patternx ...)
            MCell dotted = Vector_Ref(pattern, pat_size);
            for (size_t i = pat_size; i != input_size; ++i)
                Integrate_Tree(Match_Pattern(dotted, Vector_Ref(input, i)),
                               result);
        }
        return result;
    }
    // another type of value, must be equal to the input to match.
    else if (Equal(pattern, input)) {
        return null_cell;
    } else {
        return false_cell;
    }
}

// Make a list containing base followed by amount ellipses. Needed in
// the process of matching templates where multiple ellipses follow a
// subtemplate.
Cell Macro_Expander::Add_Ellipses(const MCell &base, size_t amount) {
    MCell result = null_cell;
    for (size_t i = 0; i != amount; ++i)
        result = Cons(dot_dot_dot, result);
    return Cons(base, result);
}

// Combine two Match_Status values. These indicate how a subtemplate
// related to the current value vector. If it contained identifiers
// that should match a value but there was no value for them in the
// vector, they get 'bad', if they contain only stuff that does not
// have to match anything (literal identifiers and other literals)
// they get 'not_bad', and if they have identifiers that match values
// they get 'good'.
Macro_Expander::Match_Status
Macro_Expander::Combine(Macro_Expander::Match_Status one,
                        Macro_Expander::Match_Status two) {
    if (one == bad || two == bad)
        return bad;
    else if (one == good || two == good)
        return good;
    else
        return not_bad;
}

// Check whether a subtemplate contains any identifiers matching stuff
// in the value vector.
Macro_Expander::Match_Status
Macro_Expander::Contains_Another_Match(MStack &values, Cell pattern) {
    if (Is_Identifier(pattern)) {
        int nr = Var_Number(pattern);
        if (nr == -1)
            return not_bad;
        else if (values[nr] == invalid_cell)
            return bad;
        else
            return good;
    } else if (Is_Pair(pattern)) {
        return Combine(Contains_Another_Match(values, Car(pattern)),
                       Contains_Another_Match(values, Cdr(pattern)));
    } else if (Is_Vector(pattern)) {
        size_t size = Vector_Size(pattern);
        Match_Status accum = not_bad;
        for (size_t i = 0; i != size; ++i) {
            accum = Combine(
                accum, Contains_Another_Match(values, Vector_Ref(pattern, i)));
            if (accum == bad)
                return bad;
        }
        return accum;
    } else {
        return not_bad;
    }
}

// Make a value vector for branch #branch_num from the values in
// source.
void Macro_Expander::Build_Subtree_Vector(MStack &target, MStack &source,
                                          size_t branch_num) {
    size_t size = _var_names.Size();
    for (size_t i = 0; i != size; ++i) {
        MCell current = source[i];
        if (Is_Pair(current) && Car(current) != invalid_cell) {
            size_t j = 0;
            for (size_t j = 0; j != branch_num && current != null_cell; ++j)
                current = Cdr(current);
            if (current == null_cell) {
                if (j == branch_num)
                    target[i] = null_cell;
                else
                    target[i] = invalid_cell;
            } else {
                target[i] = Car(current);
            }
        } else {
            target[i] = invalid_cell;
        }
    }
}

// Fill in a template with the elements that got matched. The third
// argument is there to support subtemplates in the form of (...
// template), which are treated as template except that ellipses do
// not have special meaning inside it.
Cell Macro_Expander::Fill_In_Template(MStack &var_values, const MCell &tmpl,
                                      bool use_ellipses) {
    if (Is_Pair(tmpl)) {
        MCell cdr = Cdr(tmpl);
        if (use_ellipses && Car(tmpl) == dot_dot_dot) { // (... templ)
            S_CHECK(Is_Pair(cdr) && Cdr(cdr) == null_cell,
                    "invalid use of '...' at start of list in macro template");
            return Fill_In_Template(var_values, Car(cdr), false);
        } else if (use_ellipses && Is_Pair(cdr) &&
                   Car(cdr) ==
                       dot_dot_dot) { // (templ ... [...]* . other-stuff)
            cdr = Cdr(cdr);
            MCell dotted = Car(tmpl);
            size_t ellipses = 0, branch = 0;
            List_Builder result;

            while (Is_Pair(cdr) &&
                   Car(cdr) == dot_dot_dot) { // checking for multiple ellipses
                cdr = Cdr(cdr);
                ++ellipses;
            }
            if (ellipses > 0)
                dotted = Add_Ellipses(
                    dotted, ellipses); // any extra ellipses get delegated to
                                       // the next recursion

            while (true) {
                MStack new_values(
                    _var_names.Size()); // construct a new value vector
                Build_Subtree_Vector(new_values, var_values, branch);
                if (Contains_Another_Match(new_values, dotted) != good)
                    break;
                MCell filled = Fill_In_Template(new_values, dotted, true);

                if (ellipses == 0) { // if only one set of ellipses add the
                                     // result as an atom
                    result.Add_Element(filled);
                } else { // otherwise splice it
                    for (; filled != null_cell; filled = Cdr(filled))
                        result.Add_Element(Car(filled));
                }
                ++branch;
            }

            // match the stuff after the ellipses
            result.Add_End(Fill_In_Template(var_values, cdr, true));
            return result.List();
        } else { // (templ1 . templ2)
            MCell car = Car(tmpl);
            car = Fill_In_Template(var_values, car, use_ellipses);
            cdr = Fill_In_Template(var_values, cdr, use_ellipses);
            return Cons(car, cdr);
        }
    } else if (Is_Vector(tmpl)) {
        MStack result;
        size_t size = Vector_Size(tmpl);
        for (size_t i = 0; i != size; ++i) {
            MCell current = Vector_Ref(tmpl, i);
            size_t ellipses = 0;
            if (use_ellipses) {
                while (i < size - 1 &&
                       Vector_Ref(tmpl, i + 1) ==
                           dot_dot_dot) { // immediately handle any ellipses
                                          // after this element
                    ++ellipses;
                    ++i;
                }
            }
            if (ellipses > 0) {
                MCell ellipsed = Add_Ellipses(
                    current, ellipses); // pass the ellipses to the recursion
                Cell filled = Fill_In_Template(var_values, ellipsed, true);
                for (; filled != null_cell;
                     filled = Cdr(filled)) // splice in the result
                    result.Push(Car(filled));
            } else {
                result.Push(
                    Fill_In_Template(var_values, current, use_ellipses));
            }
        }
        return Make_Vector(result);
    } else if (Is_Identifier(tmpl)) {
        int nr = Var_Number(tmpl);
        if (nr == -1) { // if this is a literal symbol it gets renamed
            return Make_Renamed_Symbol(tmpl, _macro, _unique_value);
        } else { // otherwise the value gets inserted
            Cell value = var_values[nr];
            if (value == null_cell)
                return null_cell;
            S_ASSERT(Is_Pair(value) && Car(value) == invalid_cell);
            return Cdr(value);
        }
    } else {
        // some kind of literal value
        return tmpl;
    }
}

// Expand a macro
Cell Macro_Expander::Apply(const MCell &expr) {
    // for every pattern - template match there is
    for (MCell pattern = Macro_Rules(_macro); pattern != null_cell;
         pattern = Cdr(pattern)) {
        // try a match
        Cell matched = Match_Pattern(Caar(pattern), expr);
        // if it matched
        if (matched != false_cell) {
            // make a value vector out of the match
            _var_names.Clear();
            Find_Bound_Vars(_var_names, Caar(pattern));
            MStack values(_var_names.Size());
            for (; matched != null_cell; matched = Cdr(matched)) {
                int num = Var_Number(Caar(matched));
                S_ASSERT(num != -1);
                values[num] = Cdar(matched);
            }
            // and expand the macro
            return Fill_In_Template(values, Cadar(pattern), true);
        }
    }
    // nothing matched
    throw Scheme_Error("invalid syntax for " + Symbol_Name(Macro_Name(_macro)));
}


Expander::Expander(Cell name_space, Cell local_env, size_t cur_name)
    : _cur_name(cur_name), _name_space(name_space), _local_env(local_env) {}

// Generate a new temp name that is unique in this branch.
Cell Expander::Make_Name() {
    Cell name = Make_Temp_Name(_cur_name);
    ++_cur_name;
    return name;
}

// checks for (define ....) || (begin [(define ...)]*)
bool Expander::Look_For_Defines(Cell expression, MStack &results,
                                MStack &defined_symbols) {
    static const Cell define_cell = Make_Special_Form(form_define);
    static const Cell begin_cell = Make_Special_Form(form_begin);

    MCell expanded = Macro_Expand(expression);
    Cell head = null_cell;
    if (Is_Pair(expanded) && Is_Identifier(Car(expanded)))
        head = Resolve_Name(Car(expanded), _local_env, _name_space, true);

    // if it is a define expression
    if (head == define_cell) {
        S_CHECK(Cdr(expanded) != null_cell, "invalid define expression");
        Cell symbol = Cadr(expanded);
        if (Is_Pair(symbol)) // lambda define
            symbol = Car(symbol);

        S_CHECK(Is_Identifier(symbol),
                "invalid define expression - defining a non-symbol value");
        defined_symbols.Push(symbol);
        results.Push(expanded);
        return true;
    }
    // begin, does it only contain defines?
    else if (head == begin_cell) {
        MStack parts, defined;
        for (MCell part = Cdr(expanded); part != null_cell; part = Cdr(part)) {
            S_CHECK(Is_Pair(part),
                    "malformed begin expression - not a proper list");
            MCell current = Macro_Expand(Car(part));
            if (!Look_For_Defines(current, parts, defined)) {
                results.Push(expanded);
                return false;
            }
        }
        for (size_t i = 0; i != parts.Size(); ++i)
            results.Push(parts[i]);
        for (size_t i = 0; i != defined.Size(); ++i)
            defined_symbols.Push(defined[i]);
        return true;
    }
    // other expression, is not a define
    else {
        results.Push(expanded);
        return false;
    }
}

// Expand a lambda body. Checks for internal defines. If wrap is true
// it will assume the lambda had no arguments and wrap it into a begin
// if no defines were present or a lambda with no args if there were
// defines.
Cell Expander::Expand_Body(Cell expression, bool wrap) {
    static const Cell begin_cell = Make_Special_Form(form_begin);
    static const Cell define_cell = Make_Special_Form(form_define);
    static const Cell lambda_cell = Make_Special_Form(form_lambda);

    size_t defines = 0;
    bool in_defines = true;
    MStack result, defined_symbols;
    MCell expr = expression;

    for (; expr != null_cell; expr = Cdr(expr)) {
        S_CHECK(Is_Pair(expr),
                "improper list in begin/lambda block: " + Cell_To_String(expr));

        if (in_defines) {
            defines = result.Size();
            in_defines = Look_For_Defines(Car(expr), result, defined_symbols);
        } else {
            result.Push(Macro_Expand(Car(expr)));
        }
    }

    // update the environment and gather new names
    MStack new_names;
    for (size_t i = 0; i != defined_symbols.Size(); ++i) {
        Cell symbol = defined_symbols[i];
        Cell current_binding =
            Resolve_Name(symbol, _local_env, _name_space, false, true);
        if (current_binding == symbol) {
            Cell name = Make_Name();
            Car(_local_env) = Cons(Cons(symbol, name), Car(_local_env));
            new_names.Push(name);
        }
    }

    // now that the environment is up to date we can expand all the
    // expression.
    List_Builder expanded;
    for (size_t i = 0; i != result.Size(); ++i) {
        if (i < defines)
            expanded.Add_Element(
                Expand_Form(define_cell, Cdr(result[i]), true));
        else
            expanded.Add_Element(Expand(result[i]));
    }

    if (!wrap) {
        return Cons(Make_Vector(new_names),
                    expanded.List()); // (#(new defs) body ...)
    } else if (new_names.Empty()) {   // (begin body ...)
        return Cons(begin_cell, expanded.List());
    } else { // ((lambda () #(new defs) body ...))
        MCell tmp = Make_Vector(new_names);
        tmp = Cons(tmp, expanded.List());
        tmp = Cons(null_cell, tmp);
        tmp = Cons(lambda_cell, tmp);
        return Cons_Null(tmp);
    }
}

// keep expanding macros as long as expression is a list and its first
// element refers to a macro
Cell Expander::Macro_Expand(Cell expression) {
    if (Is_Pair(expression) && Is_Identifier(Car(expression))) {
        Cell val = Resolve_Name(Car(expression), _local_env, _name_space, true);
        if (Is_Macro(val)) {
            MCell expr = Cdr(expression);
            Macro_Expander m_expander(val);
            return Macro_Expand(m_expander.Apply(expr));
        } else if (Is_Simple_Macro(val)) {
            MCell expr = Cdr(expression);
            return Macro_Expand(Expand_Simple_Macro(val, expr));
        }
    }
    return expression;
}

// handle special forms. this does a lot of error checking and other
// stuff to make the compiler's life a little simple. main function is
// to properly replace symbols with temp names when they are local.
Cell Expander::Expand_Form(Cell syntax, Cell tail, bool define_allowed) {
    static const Cell lambda = Make_Symbol("lambda");

    MCell back = tail;
    size_t args = List_Length(back, "attempt to evaluate a special form with "
                                    "an improper list as arguments");
    List_Builder result;
    result.Add_Element(syntax);

    switch (Special_Form_Name(syntax)) {
        // if - a third argument gets added if only two were given
    case form_if: {
        S_CHECK(args > 1 && args < 4, "if takes either 2 or 3 arguments");
        result.Add_Element(Expand(Car(back)));
        result.Add_Element(Expand(Cadr(back)));
        if (args == 2)
            result.Add_Element(void_cell);
        else
            result.Add_Element(Expand(Caddr(back)));
        return result.List();
    } break;

        // define - (define (a b ...) body ...) becomes (define new-a old-a
        // (lambda (b ...) body ...)) the environment is already updated by
        // Expand_Body if this is a local define. a new argument is added to
        // indicate the old name of the symbol, so that the compiler can add
        // names to functions.
    case form_define: {
        S_CHECK(args > 1, "define takes at least two arguments");
        S_CHECK(define_allowed || _local_env == null_cell,
                "use of define in incorrect position");
        MCell symbol, value;

        if (!Is_Pair(Car(back))) { // normal define
            symbol = Car(back);
            S_CHECK(args < 3, "too many arguments for define");
            value = Cadr(back);
        } else { // lambda define
            value = Cdar(back);
            value = Cons(value, Cdr(back));
            value = Cons(lambda, value);
            symbol = Caar(back);
        }
        S_CHECK(Is_Identifier(symbol), "attempt to define non-symbol value");

        if (_local_env != null_cell) {
            Cell current_binding =
                Resolve_Name(symbol, _local_env, _name_space, false, true);
            S_ASSERT(current_binding != symbol);
            S_CHECK(Is_Temp_Name(current_binding), "invalid syntax for define");
            result.Add_Element(current_binding);
        } else {
            result.Add_Element(Extract_Symbol(symbol));
        }
        result.Add_Element(Extract_Symbol(symbol));

        result.Add_Element(Expand(value));
        return result.List();
    } break;

        // set - finds out whether the symbol given is local or top-level,
        // and renames it properly if it is local
    case form_set: {
        S_CHECK(args == 2, "set! takes two arguments");
        MCell symbol = Car(back);
        S_CHECK(Is_Identifier(symbol),
                "first argument to set! must be a symbol");
        MCell value = Cadr(back);

        symbol = Resolve_Name(symbol, _local_env, _name_space, false);
        result.Add_Element(symbol);
        result.Add_Element(Expand(value));
        return result.List();
    } break;

        // lambda - used expand_body to handle internal defines. an extra
        // argument is added to indicate the number of defines. arguments
        // are renamed.
    case form_lambda: {
        S_CHECK(args >= 2, "lambda takes at least two arguments");

        // Change the argument symbols to temp names.
        List_Builder names, new_env;
        for (MCell arg_list = Car(back); arg_list != null_cell;
             arg_list = Cdr(arg_list)) {
            bool stop = false;
            MCell cur_arg;
            if (Is_Pair(arg_list)) {
                cur_arg = Car(arg_list);
            } else {
                cur_arg = arg_list;
                stop = true;
            }

            S_CHECK(Is_Identifier(cur_arg),
                    "non-symbol value in argument list");
            Cell name = Make_Name();
            new_env.Add_Element(Cons(cur_arg, name));
            if (stop) {
                names.Add_End(name);
                break;
            } else {
                names.Add_Element(name);
            }
        }

        // a new expander with the arguments in its environment is used
        // to expand the body.
        Cell new_local_env = Cons(new_env.List(), _local_env);
        Expander new_expander(_name_space, new_local_env, _cur_name);

        result.Add_Element(names.List());
        result.Add_End(new_expander.Expand_Body(Cdr(back), false));
        return result.List();
    } break;

        // begin - Expand_Body is used to make this either a begin or a
        // lambda based on whether there are internal defines.
    case form_begin: {
        // top level begin
        if (_local_env == null_cell) {
            for (; back != null_cell; back = Cdr(back))
                result.Add_Element(Expand(Car(back)));
            return result.List();
        }
        // non-inline-define begin inside a lambda
        else {
            Expander new_expander(_name_space, Cons(null_cell, _local_env),
                                  _cur_name);
            return new_expander.Expand_Body(back, true);
        }
    } break;

        // quote - identifiers renamed by macro-expansion have to be
        // un-renamed.
    case form_quote: {
        S_CHECK(args == 1, "quote takes exactly one argument");
        result.Add_Element(Unrename_Expression(Car(back)));
        return result.List();
    } break;

        // quasiquote - gathers the unquoted parts into a vector,
        // unrenames the quoted expression.
    case form_quasiquote: {
        S_CHECK(args == 1, "quasiquote only takes one argument");
        MStack unquoted;
        Find_Unquoted_Parts(unquoted, Car(back), 0);

        for (size_t i = 0; i != unquoted.Size(); ++i)
            unquoted[i] = Expand(unquoted[i]);

        result.Add_Element(Make_Vector(unquoted));
        result.Add_Element(Unrename_Expression(Car(back)));
        return result.List();
    } break;

        // old-style macro
    case form_define_macro: {
        S_CHECK(args == 2, "define-macro takes exactly one argument");
        Cell symbol = Car(back);
        S_CHECK(Is_Identifier(symbol),
                "define-syntax requires a symbol as first argument");
        symbol = Extract_Symbol(symbol);
        result.Add_Element(symbol);

        result.Add_Element(Expand(Cadr(back)));
        return result.List();
    }

        // define-syntax - already turns the second argument into a macro
    case form_define_syntax: {
        S_CHECK(args == 2, "define-syntax takes exactly two arguments");
        Cell symbol = Car(back);
        S_CHECK(Is_Identifier(symbol),
                "define-syntax requires a symbol as first argument");
        symbol = Extract_Symbol(symbol);
        result.Add_Element(symbol);

        back = Cadr(back);
        S_CHECK(Is_Syntax_Rules(back), "define-syntax requires a syntax-rules "
                                       "expression as second argument");
        result.Add_Element(
            Syntax_Rules_To_Macro(Cdr(back), symbol, null_cell, _name_space));
        return result.List();
    } break;

        // let-syntax and letrec-syntax - compiles its body in a new expander
        // with an environment that contains the new macros
    case form_let_syntax:
    case form_letrec_syntax: {
        bool letrec = Special_Form_Name(syntax) == form_letrec_syntax;
        std::string form_name = letrec ? "letrec-syntax" : "let-syntax";
        S_CHECK(args > 1, form_name + " needs more than one argument");
        MCell defs = Car(back);
        S_CHECK(Is_Pair(defs), "invalid definition list for " + form_name);
        List_Builder new_macros;
        MCell env_used;
        if (letrec)
            env_used = Cons(null_cell, _local_env);
        else
            env_used = _local_env;

        for (; defs != null_cell; defs = Cdr(defs)) {
            S_CHECK(Is_Pair(defs),
                    "definition list in " + form_name + " not a valid list");
            S_CHECK(Is_Pair(Car(defs)) && Is_Identifier(Caar(defs)),
                    "improper element in definition list for " + form_name);
            S_CHECK(Is_Pair(Cdar(defs)) && Is_Syntax_Rules(Cadar(defs)),
                    "values for all definitions in " + form_name +
                        " must be syntax-rules expressions");

            Cell symbol = Caar(defs);
            MCell macro =
                Syntax_Rules_To_Macro(Cdr(Cadar(defs)), Extract_Symbol(symbol),
                                      env_used, _name_space);
            if (letrec)
                Car(env_used) = Cons(Cons(symbol, macro), Car(env_used));
            else
                new_macros.Add_Element(Cons(symbol, macro));
        }

        if (!letrec)
            env_used = Cons(new_macros.List(), _local_env);
        Expander new_expander(_name_space, env_used, _cur_name);
        return new_expander.Expand_Body(Cdr(back), true);
    } break;

        // does nothing
    case form_current_env:
        S_CHECK(args == 0, "current-env takes no arguments");
        return result.List();
        break;

    default:; //
    }

    throw Scheme_Error("unknown special form");
}

// Expand an arbitraty expression
Cell Expander::Expand(Cell expression) {
    // First macro-expand it
    expression = Macro_Expand(expression);
    if (Is_Pair(expression)) {
        // decide whether it is a special form or a function call
        Cell head = Car(expression);
        if (Is_Identifier(head))
            head = Resolve_Name(head, _local_env, _name_space, true);

        if (Is_Special_Form(head)) {
            return Expand_Form(head, Cdr(expression), false);
        }
        // Simply expand every element of a list an return a list of those
        // elements. Checks for improper lists.
        else {
            MCell expr = expression;
            List_Builder new_list;
            for (MCell lst = expression; lst != null_cell; lst = Cdr(lst)) {
                S_CHECK(Is_Pair(lst), "can not evaluate improper list: " +
                                          Cell_To_String(expr));
                new_list.Add_Element(Expand(Car(lst)));
            }
            return new_list.List();
        }
    }
    // identifiers get resolved
    else if (Is_Identifier(expression)) {
        Cell resolved = Resolve_Name(expression, _local_env, _name_space, true);
        S_CHECK(!Is_Syntax(resolved),
                "invalid syntax for " +
                    Symbol_Name(Extract_Symbol(expression)));
        return resolved;
    }
    // nothing happens to other stuff
    else {
        return expression;
    }
}

} // namespace uls
