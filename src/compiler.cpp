#include "compiler.hpp"
#include "form.hpp"
#include "interpreter.hpp"
#include "instruction.hpp"
#include "primitive.hpp"
#include "closure.hpp"
#include "string.hpp"
#include "namespace.hpp"
#include "list.hpp"

namespace uls {

Call_Primitive call_primitive_table[] = {
    Call_Primitive_0, Call_Primitive_1, Call_Primitive_2,
    Call_Primitive_3, Call_Primitive_4, Call_Primitive_5,
    Call_Primitive_6, Call_Primitive_7, Call_Primitive_8};

void Push_Continuation(Live_Continuation &cont, MStack &stack);
void Pop_Continuation(Live_Continuation &cont, MStack &stack);

// ,SIGINT CATCHER

// Sets a flag when ctrl-C is pressed. If ctrl-C is pressed twice
// without this thing polling in between the second signal is let
// through.
class SigINT_Catcher {
  public:
    SigINT_Catcher() {
        interrupted = false;
        Register();
    }
    ~SigINT_Catcher() { Unregister(); }

    void Poll() {
        if (interrupted) {
            interrupted = false;
            Register();
            throw Scheme_Error("user break");
        }
    }

  private:
    static void Register() { old_handler = std::signal(SIGINT, Handler); }
    static void Unregister() { std::signal(SIGINT, old_handler); }

    static void (*old_handler)(int);
    static volatile bool interrupted;
    static void Handler(int signal);
};

void (*SigINT_Catcher::old_handler)(int) = nullptr;
volatile bool SigINT_Catcher::interrupted = false;
void SigINT_Catcher::Handler(int signal) {
    interrupted = true;
    Unregister();
}


// The 'virtual machine', fetches instructions from its current code
// vector and executes them.
Cell Run_Code(Cell code, bool handle_errors) {
    SigINT_Catcher catcher;

    S_ASSERT(Is_Vector(code));
    Live_Continuation lc(code);
    MStack stack; // continuations are kept on this
    bool done = false;

    // the eval loop, fetches instructions and executes code based on them
    while (!done) {
        try {
            catcher.Poll();

            S_ASSERT(lc.instruction_counter < Vector_Size(lc.code));
            Cell current = Vector_Ref(lc.code, lc.instruction_counter);
            S_ASSERT(Is_Instruction(current));
            ++lc.instruction_counter;
            Instruction current_instruction = Instruction_Value(current);

            switch (current_instruction) {
            case i_return: // return from current continuation (reg is return
                           // value)
                if (stack.Empty())
                    done = true;
                else
                    Pop_Continuation(lc, stack);
                break;

            case i_literal: // put the next element in the code vector in reg
                lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
                ++lc.instruction_counter;
                break;

            case i_jump: // jump to the instruction indicated by the next
                         // element in the code vector
                lc.instruction_counter =
                    Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
                break;

            case i_jump_if_false: // if reg is false, jump to the instruction
                                  // indicated by the next element in the code
                                  // vector
                if (lc.reg == false_cell)
                    lc.instruction_counter = Fixnum_Value(
                        Vector_Ref(lc.code, lc.instruction_counter));
                else
                    ++lc.instruction_counter;
                break;

            case i_tail: // perform a tail call with the closure or primitive in
                         // reg
            case i_call: // call the closure or primitive in reg
                // these had so much code in common I made them one case, it is
                // checked again inside the code whether a normal call or a tail
                // call is happening
                {
                    S_ASSERT(lc.arguments != null_cell);
                    Cell arguments = Car(lc.arguments);
                    lc.arguments = Cdr(lc.arguments);
                    int values =
                        (arguments == null_cell ? 0 : Vector_Size(arguments));

                    if (Is_Primitive(lc.reg)) { // primitive call
                        int args = Primitive_Num_Args(lc.reg);
                        Cell arg_vector = Adjust_Arguments(
                            arguments, values, args, Primitive_Name(lc.reg));
                        Cell *arg_array = nullptr;
                        if (args != 0)
                            arg_array = &Vector_Ref(arg_vector, 0);
                        if (args < 0)
                            args = -args;
                        S_ASSERT(args <= 8);
                        lc.reg = call_primitive_table[args](
                            Primitive_Function(lc.reg), arg_array);

                        // return
                        if (current_instruction == i_tail) {
                            if (stack.Empty())
                                done = true;
                            else
                                Pop_Continuation(lc, stack);
                        }
                    } else if (Is_Closure(lc.reg)) { // closure call
                        MCell arg_vec = Adjust_Arguments(
                            arguments, values, Closure_Num_Args(lc.reg),
                            Closure_Name(lc.reg));
                        if (current_instruction == i_call)
                            Push_Continuation(lc, stack);

                        // change the live environment to the start of this
                        // closure
                        lc.environment =
                            Cons(arg_vec, Closure_Environment(lc.reg));
                        lc.code = Closure_Code(lc.reg);
                        lc.instruction_counter = 0;
                        lc.function_name = Closure_Name(lc.reg);
                    } else { // invalid call
                        throw Scheme_Error(
                            "attempt to call non-function object: " +
                            Cell_To_String(lc.reg));
                    }
                }
                break;

            case i_finish_lambda: // make a closure with the current environment
                                  // and the pair (args . code) in reg
                S_ASSERT(Is_Vector(lc.reg));
                lc.reg = Make_Closure(lc.reg, lc.environment);
                break;

            case i_grow_env: // increase the size of the local environment to
                             // make room for newly defined symbols
            {
                int new_size =
                    Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
                ++lc.instruction_counter;
                Grow_Environment(lc.environment, new_size);
            } break;

            case i_deref_env: // lookup the value indicated by the next element
                              // in the code vector in the environment and put
                              // in in reg
            {
                lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
                ++lc.instruction_counter;
                S_ASSERT(Is_Pair(lc.reg));
                Cell target_env = lc.environment;

                for (size_t depth = Fixnum_Value(Car(lc.reg)); depth != 0;
                     --depth) {
                    S_ASSERT(Cdr(target_env) != null_cell);
                    target_env = Cdr(target_env);
                }
                target_env = Car(target_env);
                size_t distance = Fixnum_Value(Cdr(lc.reg));
                S_ASSERT(Is_Vector(target_env));
                S_ASSERT(distance < Vector_Size(target_env));

                lc.reg = Vector_Ref(target_env, distance);
                S_CHECK(lc.reg != invalid_cell,
                        "accessing locally-defined symbol before its "
                        "initialization");
            } break;

            case i_deref_ref: // look up the value indicated the next element on
                              // the code vector in the top environment and put
                              // it in reg
                lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
                ++lc.instruction_counter;
                S_ASSERT(Is_Pair(lc.reg));
                S_CHECK(Cdr(lc.reg) != invalid_cell,
                        "undefined symbol: " + Symbol_Name(Car(lc.reg)));
                lc.reg = Cdr(lc.reg);
                break;

            case i_set_env: // set environment var indicated by next element in
                            // code vector to the value of reg
            {
                Cell ref = Vector_Ref(lc.code, lc.instruction_counter);
                ++lc.instruction_counter;
                size_t depth = Fixnum_Value(Car(ref)),
                       distance = Fixnum_Value(Cdr(ref));
                S_ASSERT(Is_Pair(ref));

                Cell target_env = lc.environment;
                for (; depth > 0; --depth) {
                    S_ASSERT(Cdr(target_env) != null_cell);
                    target_env = Cdr(target_env);
                }

                Vector_Ref(Car(target_env), distance) = lc.reg;
                lc.reg = void_cell;
            } break;

            case i_define_ref: // define cell indicated by next element in code
                               // vector to value of reg
            case i_set_ref: // set cell indicated by next element in code vector
                            // to value of reg
            {
                Cell ref = Vector_Ref(lc.code, lc.instruction_counter);
                ++lc.instruction_counter;
                S_ASSERT(Is_Pair(ref));
                if (current_instruction ==
                    i_set_ref) { // for set!, check whether the symbol is bound
                    S_CHECK(Cdr(ref) != invalid_cell,
                            "attempt to set undefined symbol: " +
                                Symbol_Name(Car(ref)));
                }
                Cdr(ref) = lc.reg;
                lc.reg = void_cell;
            } break;

            case i_setup_arg_list: // push a new arg list whose size is
                                   // indicated in the next element of the code
                                   // vector
            {
                size_t list_size =
                    Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
                ++lc.instruction_counter;
                MCell new_vector = null_cell;
                if (list_size != 0)
                    new_vector = Make_Vector(list_size, invalid_cell);
                lc.arguments = Cons(new_vector, lc.arguments);
            } break;

            case i_add_arg: // add the value in reg to the top arg list at the
                            // position indicated by the next element
            {
                size_t arg_num =
                    Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
                ++lc.instruction_counter;
                S_ASSERT(lc.arguments != null_cell);
                S_ASSERT(Car(lc.arguments) != null_cell);
                Vector_Ref(Car(lc.arguments), arg_num) = lc.reg;
            } break;

            case i_as_arguments: // set the top arg list to the elements of the
                                 // list in reg (used by apply)
                if (lc.reg != null_cell)
                    lc.reg = Make_Vector_From_List(lc.reg);
                lc.arguments = Cons(lc.reg, lc.arguments);
                break;

            case i_make_macro: // turn the closure in reg into a macro
                lc.reg = Make_Simple_Macro(lc.reg);
                break;

            case i_current_continuation: // load the current continuation to reg
            {
                MCell current = null_cell;
                for (size_t i = 0; i != stack.Size(); i += continuation_size)
                    current = Make_Continuation(stack, i, current);
                lc.reg = current;
            } break;

            case i_set_continuation: // restore the continuation found in reg
            {
                S_ASSERT(lc.reg == null_cell || Is_Continuation(lc.reg));
                stack.Clear();
                std::vector<Cell> continuations;
                while (lc.reg != null_cell) {
                    continuations.push_back(lc.reg);
                    lc.reg = Continuation_Parent(lc.reg);
                }
                for (size_t i = continuations.size(); i != 0; --i) {
                    Restore_Continuation(continuations[i - 1], stack);
                }
            } break;

            default:
                throw Scheme_Error("invalid instruction");
                break;
            }
        } catch (Scheme_Error &e) {
            const static Cell handle_error = Make_Symbol("impl:handle-error");
            const static Cell quote_cell = Make_Symbol("quote");

            // when an error occurs impl:handle-error is called with some
            // context information if handl_errors is true and
            // impl:handle-error is defined
            if (handle_errors &&
                Get_Value(Interpreter::ReportEnv(), handle_error) != invalid_cell) {
                MCell instructions = lc.code;
                size_t current_instruction = (lc.instruction_counter == 0)
                                                 ? 0
                                                 : (lc.instruction_counter - 1);

                // Make a list of function names for simple stack tracing.
                List_Builder trace;
                trace.Add_Element(lc.function_name);
                for (size_t pos = stack.Size(); pos != 0; pos -= 5)
                    trace.Add_Element(stack[pos - 2]);

                // Reset the context
                stack.Clear();
                lc.arguments = null_cell;
                lc.environment = null_cell;
                lc.function_name = void_cell;
                lc.instruction_counter = 0;

                // Run (impl:handle-error <error message> <stack trace> <current
                // instruction> <instruction vector>)
                MCell command = Cons_Null(instructions);
                command = Cons(Make_Fixnum(current_instruction), command);
                command =
                    Cons(Cons(quote_cell, Cons_Null(trace.List())), command);
                command = Cons(Make_String(e.what()), command);
                command = Cons(handle_error, command);
                lc.code = Compile(command, Interpreter::ReportEnv());
            }
            // otherwise the exception is not handled here
            else {
                throw;
            }
        }
    }

    return lc.reg;
}


// compile actually does both expanding and compiling. returns a code
// vector. the third argument is needed to support evaluating in
// different environments.
Cell Compile(Cell expression, const MCell &name_space) {
    Expander expander(name_space, null_cell, 0);
    Cell expanded = expander.Expand(expression);
    Compiler compiler(name_space, null_cell);
    compiler.Compile(expanded, true);
    return compiler.Code();
}


Cell Make_Macro(const MCell &specials, const MCell &rules, Cell name,
                const MCell &local_env, const MCell &name_space) {
    Cell new_macro = Allocate_Cell<Macro_Data>(macro_type);
    Macro_Data &data = Extract<Macro_Data>(new_macro);
    data.special_symbols = specials;
    data.rules = rules;
    data.name = name;
    data.local_env = local_env;
    data.name_space = name_space;
    return new_macro;
}

Live_Continuation::Live_Continuation(Cell start_code)
    : code(start_code), function_name(false_cell), instruction_counter(0) {}

// This pushing and popping of continuations is slightly crummy,
// information about which field goes where is present here, in the
// Run_Code loop and in the Restore_Continuation function.
//const size_t continuation_size = 5;

void Push_Continuation(Live_Continuation &cont, MStack &stack) {
    stack.Push(cont.code);
    stack.Push(cont.arguments);
    stack.Push(cont.environment);
    stack.Push(cont.function_name);
    stack.Push(Make_Fixnum(cont.instruction_counter));
}

void Pop_Continuation(Live_Continuation &cont, MStack &stack) {
    cont.instruction_counter = Fixnum_Value(stack.Pop());
    cont.function_name = stack.Pop();
    cont.environment = stack.Pop();
    cont.arguments = stack.Pop();
    cont.code = stack.Pop();
}

Cell Make_Continuation(MStack &stack, size_t position, const MCell &parent) {
    Cell new_c = Allocate_Cell<Continuation_Data>(continuation_type);
    Continuation_Data &data = Extract<Continuation_Data>(new_c);
    data.code = stack[position];
    data.arguments = stack[++position]; // ewwwwww... it is fast though
    data.environment = stack[++position];
    data.function_name = stack[++position];
    data.code_counter = stack[++position];
    data.continuation = parent;

    return new_c;
}

void Restore_Continuation(Cell continuation,
                          MStack &stack) // TODO: copy argument vectors
{
    Continuation_Data &data = Extract<Continuation_Data>(continuation);
    stack.Push(data.code);
    stack.Push(data.arguments);
    stack.Push(data.environment);
    stack.Push(data.function_name);
    stack.Push(data.code_counter);
}


Compiler::Compiler(Cell name_space, Cell environment)
    : _name_space(name_space), _environment(environment),
      _defining_symbol(false_cell) {}

// macro to make adding instructions simple
#define INS(name) _code.Push(Make_Instruction(i_##name))

// compile any expression. note that this assumes it has already been
// expanded. will not do much good on non-expanded expressions.
void Compiler::Compile(Cell expression, bool tail) {
    if (Is_Pair(expression)) {
        if (Is_Special_Form(Car(expression)))
            Compile_Special_Form(Car(expression), Cdr(expression), tail);
        else
            Compile_Function_Call(expression, tail);
    }
    // because local symbols have become temp names, symbols always mean
    // top-level bindings
    else if (Is_Symbol(expression)) {
        Cell slot = Get_Binding(_name_space, expression);
        // Symbols pointing to syntax can not be dereferenced
        S_CHECK(!Is_Syntax(Cdr(slot)),
                "invalid syntax for " + Symbol_Name(Car(slot)));
        INS(deref_ref);
        _code.Push(slot);
        if (tail)
            INS(return );
    }
    // a temp name means a local variable
    else if (Is_Temp_Name(expression)) {
        Environment_Ref ref = Find_In_Environment(expression, _environment);
        S_ASSERT(ref.depth != not_found);
        INS(deref_env);
        _code.Push(Make_Env_Ref(ref.depth, ref.distance));
        if (tail)
            INS(return );
    }
    // other stuff is literal
    else {
        S_CHECK(expression != null_cell, "attempt to evaluate empty list");
        INS(literal);
        _code.Push(expression);
        if (tail)
            INS(return );
    }
}

// Compile a function call. First the arguments get compiled left to
// right and added to the arg list, and then the first element of the
// list is compiled and the function is called.
void Compiler::Compile_Function_Call(Cell expression, bool tail) {
    MCell head = Car(expression), back = Cdr(expression);

    INS(setup_arg_list);
    _code.Push(Make_Fixnum(List_Length(back)));
    size_t arg_num = 0;
    while (back != null_cell) {
        Compile(Car(back), false);
        back = Cdr(back);

        INS(add_arg);
        _code.Push(Make_Fixnum(arg_num));
        ++arg_num;
    }
    Compile(head, false);
    if (tail)
        INS(tail);
    else
        INS(call);
}

// compile special forms
void Compiler::Compile_Special_Form(Cell syntax, Cell arguments, bool tail) {
    static const Cell lambda = Make_Special_Form(form_lambda);
    static const Cell fill_in_quasiquoted =
        Make_Symbol("impl:fill-in-quasiquoted");

    MCell back = arguments;

    switch (Special_Form_Name(syntax)) {
    case form_if:
        // straightforward jump construction
        {
            Compile(Car(back), false);
            INS(jump_if_false);
            size_t first_jump = _code.Size();
            _code.Push(invalid_cell);
            Compile(Cadr(back), tail);
            size_t second_jump = 0;
            if (!tail) {
                INS(jump);
                second_jump = _code.Size();
                _code.Push(invalid_cell);
            }
            _code[first_jump] = Make_Fixnum(_code.Size());
            Compile(Caddr(back), tail);
            if (!tail) {
                _code[second_jump] = Make_Fixnum(_code.Size());
            }
        }
        break;

    case form_define:
        // depening on whether this is at top level either a top level var
        // or a var in the current local environment gets defined. if the
        // value is a lambda expression _defining_symbol gets set, which
        // causes the next compiled lambda to set the name of the new
        // closure.
        {
            MCell plain_symbol = Cadr(back), renamed_symbol = Car(back),
                  value = Caddr(back);

            if (Is_Pair(value) && Car(value) == lambda)
                _defining_symbol = plain_symbol;

            Compile(value, false);

            if (_environment == null_cell) {
                S_ASSERT(Is_Symbol(renamed_symbol));
                INS(define_ref);
                _code.Push(Get_Binding(_name_space, renamed_symbol));
            } else {
                S_ASSERT(Is_Temp_Name(renamed_symbol));
                INS(set_env);
                _code.Push(Make_Env_Ref(
                    0, Environment_Offset(renamed_symbol, Car(_environment))));
            }
            if (tail)
                INS(return );
        }
        break;

    case form_set:
        // if symbol is a symbol this is a top level var, otherwise it is
        // some local var
        {
            Cell symbol = Car(back), value = Cadr(back);
            Compile(value, false);

            if (Is_Symbol(symbol)) {
                INS(set_ref);
                _code.Push(Get_Binding(_name_space, symbol));
            } else {
                Environment_Ref ref = Find_In_Environment(symbol, _environment);
                S_ASSERT(ref.depth != not_found);
                INS(set_env);
                _code.Push(Make_Env_Ref(ref.depth, ref.distance));
            }
            if (tail)
                INS(return );
        }
        break;

    case form_lambda:
        // actually creates an unfinished closure that gets finished at
        // runtime.
        {
            size_t num_args = 0;
            bool var_args = false;
            Cell arg_list = Car(back);
            // count the arguments and decide whether there is a 'rest'
            // argument
            while (arg_list != null_cell) {
                ++num_args;
                if (!Is_Pair(arg_list)) {
                    var_args = true;
                    break;
                }
                arg_list = Cdr(arg_list);
            }

            // put the arguments into a vector, this will be part of the
            // environment in which the body gets compiled.
            MCell arg_vec = Make_Vector(num_args);
            arg_list = Car(back);
            for (size_t i = 0; i != num_args; ++i) {
                if (Is_Pair(arg_list)) {
                    Vector_Ref(arg_vec, i) = Car(arg_list);
                    arg_list = Cdr(arg_list);
                } else {
                    S_ASSERT(i == num_args - 1);
                    Vector_Ref(arg_vec, i) = arg_list;
                }
            }

            // the body gets compiled in a new compiler
            Compiler new_code(_name_space, Cons(arg_vec, _environment));

            MCell new_defines = Cadr(back), body = Cddr(back);

            // adjust the environment for the defines in the body
            if (!Vector_Size(new_defines) == 0) {
                size_t old_size = Vector_Size(Car(new_code._environment)),
                       added_size = Vector_Size(new_defines),
                       new_size = old_size + added_size;
                new_code._code.Push(Make_Instruction(i_grow_env));
                new_code._code.Push(Make_Fixnum(new_size));

                Grow_Environment(new_code._environment, new_size);
                for (size_t i = 0; i != added_size; ++i)
                    Vector_Ref(Car(new_code._environment), old_size + i) =
                        Vector_Ref(new_defines, i);
            }

            // compile the body itself
            for (; body != null_cell; body = Cdr(body))
                new_code.Compile(Car(body), Cdr(body) == null_cell);

            // some code for finishing the closure
            INS(literal);
            MCell new_code_vector = new_code.Code();
            int arg_code = num_args;
            if (var_args)
                arg_code = -arg_code;
            _code.Push(Make_Unfinished_Closure(
                new_code_vector, Make_Fixnum(arg_code), _defining_symbol));
            _defining_symbol = false_cell;
            INS(finish_lambda);

            if (tail)
                INS(return );
        }
        break;

    case form_begin:
        // begins get inlined
        if (back == null_cell) {
            INS(literal);
            _code.Push(void_cell);
        } else {
            for (; back != null_cell; back = Cdr(back))
                Compile(Car(back), tail && Cdr(back) == null_cell);
        }
        if (tail)
            INS(return );
        break;

    case form_quote:
        INS(literal);
        _code.Push(Car(back));
        if (tail)
            INS(return );
        break;

    case form_quasiquote:
        // this becomes a call to impl:fill-in-quasiquoted, with the
        // expression as a first argument and the expressions resulting
        // from evaluating the unquoted parts as remaning arguments.
        {
            // the expander already gathered the unquoted parts
            MCell unquoted = Car(back);
            size_t size = Vector_Size(unquoted);

            // pass the quoted expression
            INS(setup_arg_list);
            _code.Push(Make_Fixnum(size + 1));
            INS(literal);
            _code.Push(Cadr(back));
            INS(add_arg);
            _code.Push(zero_cell);

            // and the unquoted parts
            for (size_t part = 0; part != size; ++part) {
                Compile(Vector_Ref(unquoted, part), false);
                INS(add_arg);
                _code.Push(Make_Fixnum(part + 1));
            }

            // call impl:fill-in-quasiquoted
            INS(deref_ref);
            _code.Push(Get_Binding(_name_space, fill_in_quasiquoted));
            if (tail)
                INS(tail);
            else
                INS(call);
        }
        break;

    case form_define_macro:
        // uh-oh
        {
            Cell symbol = Car(back), value = Cadr(back);
            Compile(value, false);
            INS(make_macro);
            INS(define_ref);
            _code.Push(Get_Binding(_name_space, symbol));
            if (tail)
                INS(return );
        }
        break;

    case form_define_syntax:
        // most of the work was done by the expander. just bind arg 1 to
        // arg 2.
        {
            Cell symbol = Car(back);
            INS(literal);
            _code.Push(Cadr(back));

            INS(define_ref);
            _code.Push(Get_Binding(_name_space, symbol));
            if (tail)
                INS(return );
        }
        break;

    case form_current_env:
        // just put the current env in the code vector
        INS(literal);
        _code.Push(_name_space);
        if (tail)
            INS(return );
        break;

    default:
        throw Scheme_Error("unknown special form");
    }
}

#undef INS



} // namespace uls
