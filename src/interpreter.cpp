#include <fstream>
#include "interpreter.hpp"
#include "syntax.hpp"
#include "compiler.hpp"
#include "primitive.hpp"
#include "namespace.hpp"
#include "output.hpp"
#include "version.hpp"
#include "list.hpp"
#include "string.hpp"
#include "number_io.hpp"

namespace uls {

// Global pointers to Interpreter and Mem_Manager (exists inside
// Interpreter). Are nullptr if none are alive.
Interpreter *Interpreter::ip_ = nullptr;

Cell Eval_String(const std::string &str, bool handle_errors) {
    return Eval_String(str, Interpreter::WorkEnv(), handle_errors);
}

Cell Eval_Expression(Cell expression, bool handle_errors) {
    return Eval_Expression(expression, Interpreter::WorkEnv(), handle_errors);
}

// evaluate a string. only the first expression in the string is
// evaluated ("1 2 3" => 1)
Cell Eval_String(const std::string &str, const MCell &name_space,
                 bool handle_errors) {
    String_Input_Splitter splitter;
    splitter.Add_Line(str);
    S_CHECK(splitter.Full_Expression(),
            "attempt to eval unfinished expression");
    Cell expr = splitter.Read();
    return Eval_Expression(expr, name_space, handle_errors);
}

// evaluate a cell
Cell Eval_Expression(Cell expression, Cell name_space, bool handle_errors) {
    Cell code = Compile(expression, name_space);
    return Run_Code(code, handle_errors);
}


// Get a value from a namespace and the namespaces below it without
// creating a new pair if it does not exist.
Cell Get_Value(Cell name_space, Cell symbol) {
    for (; name_space != null_cell; name_space = Namespace_Parent(name_space)) {
        for (Cell element =
                 Namespace_Ref(name_space, Hash_Symbol(symbol, name_space));
             element != null_cell; element = Cdr(element)) {
            if (Caar(element) == symbol)
                return Cdar(element);
        }
    }
    return invalid_cell;
}

Interpreter::Interpreter(size_t memory)
    : mem_manager(memory * 128),
      null_env(Make_Namespace(null_cell, null_env_size)),
      report_env(Make_Namespace(null_env, report_env_size)),
      work_env(Make_Namespace(report_env, default_workspace_size)),
      input(Make_Inport(std::cin)), output(Make_Outport(std::cout)) {
    S_CHECK(ip_ == nullptr, "only one interpreter can be alive at any time");
    ip_ = this;

    Initialize_Syntax(null_env);
    Initialize_Primitives(report_env);

    // this is to get the value of null_env to the init code
    Define_Symbol(report_env, "*null-env*", null_env);
    // bootstrap code. loads init file.
    const char *open =
        "((lambda (file target-env)"
        "  (define (loop expr)"
        "    (if (eof-object? expr)"
        "        #v"
        "        (begin"
        "          (if (eq? ((compile expr target-env)) 'goto-report-env)"
        "            (set! target-env (impl:current-env)))"
        "          (loop (read file)))))"
        "  (loop (read file))"
        "  (close-input-port file)) (open-input-file \"";
    const char *close = "\") *null-env*)";
    Eval_String(open + Find_Init_File() + close, report_env, false);
    // undef *null-env* again
    Define_Symbol(report_env, "*null-env*", invalid_cell);
    Bootstrap_Namespace(work_env);
}

Interpreter::~Interpreter() {
#ifdef WITH_DESTRUCTORS
    mem_manager.Call_All_Destructors();
#endif
    ip_ = nullptr;
}

#ifdef WITH_DESTRUCTORS
Cell_Type Make_Type(Write_Function write, Destroy_Function destruct) {
    return Interpreter::TypeManager().Make_Type(write, destruct);
}
#else
Cell_Type Make_Type(Write_Function write) {
    return Interpreter::TypeManager().Make_Type(write);
}
#endif

// run a repl using in.input and in.output as in and output.
void Interpreter::Run_REPL(bool welcome_message) {
    if (welcome_message)
        Outport_Stream(output)
            << "Welcome to " << APP_NAME << " v" << VERSION << "\n";

    Eval_String("(run-repl (impl:current-env))", work_env, true);
}

// load a file
void Load_File(const std::string &filename) {
    Eval_String("(load \"" + filename + "\")", true);
}

// non-iostream repl
bool String_REPL::Add_Line(const std::string &str) {
    bool retval = false;
    _input.Add_Line(str);
    try {
        while (_input.Full_Expression()) {
            retval = true;
            Cell next = _input.Read();
            next = Run_Code(Compile(next, Interpreter::WorkEnv()), false);
            if (next != void_cell) {
                std::ostream &output = Outport_Stream(Interpreter::Output());
                Write(next, output);
                output << '\n';
            }
        }
    } catch (const Scheme_Error &e) {
        _input.Reset();
        throw;
    }
    return retval;
}


// capture the output to in.out
Output_Catcher::Output_Catcher() : _old_stream(Interpreter::Output()) {
    Interpreter::SetOutput(Make_Outport(_stream));
}

Output_Catcher::~Output_Catcher() { Interpreter::SetOutput(_old_stream); }

std::string Output_Catcher::Get_New_Output() {
    std::string new_output = _stream.str();
    _stream.str("");
    return new_output;
}


std::string Find_Init_File() {
    // With ALWAYS_COLLECT turned on the interpreter is way too slow for
    // the full init file, so we use a smaller one (still awfully slow
    // though).
    const char *filename =
#ifdef ALWAYS_COLLECT
        "init-light.scm"
#else
#ifdef INIT_FILE
        INIT_FILE
#else
        "uscheme-init.scm"
#endif
#endif
        ;
    const char *env_name = std::getenv("USCHEME_INIT_FILE");
    std::string retval;
    if (env_name != nullptr && !std::ifstream(env_name).fail()) {
        retval = env_name;
    } else if (!std::ifstream(filename).fail()) {
        retval = filename;
    }
#ifdef PREFIX
#ifndef _WIN32
    else {
        std::string long_name = PREFIX + std::string("/share/") + filename;
        if (!std::ifstream(long_name.c_str()).fail())
            retval = long_name;
    }
#endif
#endif
    S_CHECK(retval.size() != 0, "could not locate init file");
    return retval;
}


// The error checking for macros currently happens mostly at runtime
// (as opposed to compile time), this is not very nice but it saved me
// a lot of almost-duplicate code.
Cell Syntax_Rules_To_Macro(Cell syntax_rules, Cell name,
                           const MCell &environment, const MCell &name_space) {
    S_CHECK(List_Length(syntax_rules, "improper list in syntax-rules") >= 2,
            "not enough arguments to syntax-rules");

    List_Builder pattern;
    MCell rules = Cdr(syntax_rules), specials = Car(syntax_rules);

    for (; rules != null_cell; rules = Cdr(rules)) {
        S_CHECK(Is_Pair(Car(rules)) && Caar(rules) != null_cell,
                "pattern rule in syntax-rules is not a valid rule");

        pattern.Add_Element(Cons(Cdaar(rules), Cdar(rules)));
    }

    return Make_Macro(specials, pattern.List(), name, environment, name_space);
}


// ,ENVIRONMENT



// Does two things. Checks whether a correct number of arguments was
// given, and in case the last argument is a 'rest' argument it
// changes the vector around to move the rest arguments into a list.
Cell Adjust_Arguments(Cell argument_vector, int num_values, int num_args,
                      Cell function_name) {
    if (num_args >= 0) { // fixed argument number
        if (num_values == num_args)
            return argument_vector;
        else if (num_values < num_args)
            throw Scheme_Error("not enough arguments to function " +
                               Function_Name(function_name));
        else
            throw Scheme_Error("too many arguments to function " +
                               Function_Name(function_name));
    } else { // variable argument number
        num_args = -num_args;
        if (num_values < num_args - 1) {
            throw Scheme_Error("not enough arguments to function " +
                               Function_Name(function_name));
        } else { // construct a new argument vector from the old one, putting
                 // all the arguments past num_args - 1 into a list
            MCell old_arg_vec = argument_vector,
                  new_arg_vec = Make_Vector(num_args);
            for (int i = 0; i < num_args - 1; ++i)
                Vector_Ref(new_arg_vec, i) = Vector_Ref(old_arg_vec, i);
            List_Builder var_arg_list;
            for (int i = num_args - 1; i < num_values; ++i)
                var_arg_list.Add_Element(Vector_Ref(old_arg_vec, i));
            Vector_Ref(new_arg_vec, num_args - 1) = var_arg_list.List();
            return new_arg_vec;
        }
    }
}

// Increase the size of the top vector of an environment. Used for
// local defines.
void Grow_Environment(const MCell &environment, size_t new_size) {
    Cell new_vec = Make_Vector(new_size, invalid_cell);
    Cell old_vector = Car(environment);
    if (old_vector != null_cell) {
        size_t old_size = Vector_Size(old_vector);
        S_ASSERT(new_size >= old_size);

        for (size_t i = 0; i != old_size; ++i)
            Vector_Ref(new_vec, i) = Vector_Ref(old_vector, i);
    }
    Car(environment) = new_vec;
}


// ,TYPES

// Thingy to keep track of what print functions belong to what types.
// Registers all the standard types on startup.
Type_Manager::Type_Manager()
    : _functions(available_type, nullptr),
#ifdef WITH_DESTRUCTORS
      _destructors(available_type, nullptr),
#endif
      _current(available_type) {
    struct Association {
        Cell_Type type;
        Write_Function write;
    };

    static const Association init[] = {
        {renamed_symbol_type, Write_Renamed_Symbol},
        {pair_type, Write_Pair},
        {vector_type, Write_Vector},
        {string_type, Write_String},
        {closure_type, Write_Closure},
        {primitive_type, Write_Primitive},
        {continuation_type, Write_Continuation},
        {inport_type, Write_Inport},
        {outport_type, Write_Outport},
        {rational_type, Write_Rational},
        {real_type, Write_Real},
        {bignum_type, Write_Bignum},
        {macro_type, Write_Macro},
        {simple_macro_type, Write_Macro},
        {namespace_type, Write_Namespace},
        {available_type, nullptr}};
    for (const Association *cur = init; cur->write != nullptr; ++cur)
        _functions[cur->type] = cur->write;
#ifdef WITH_DESTRUCTORS
    _destructors[inport_type] = Close_Inport;
    _destructors[outport_type] = Close_Outport;
#endif
}

Cell_Type Type_Manager::Make_Type(Write_Function write
#ifdef WITH_DESTRUCTORS
                                  ,
                                  Destroy_Function destruct
#endif
) {
    _functions.resize(_current + 1);
    _functions[_current] = write;
#ifdef WITH_DESTRUCTORS
    _destructors.resize(_current + 1);
    _destructors[_current] = destruct;
#endif

    S_ASSERT(_current != static_cast<Cell_Type>(max_byte));
    Cell_Type c = _current;
    _current = static_cast<Cell_Type>(_current + 1);
    return c;
}


// Extract the unquoted parts of a quasiquoted expression.
void Find_Unquoted_Parts(MStack &unquoted, Cell expression, size_t depth) {
    const static Cell unquote = Make_Symbol("unquote");
    const static Cell unquote_splicing = Make_Symbol("unquote-splicing");
    const static Cell quasiquote = Make_Symbol("quasiquote");

    if (Is_Pair(expression)) {
        if (Car(expression) == unquote || Car(expression) == unquote_splicing) {
            S_CHECK(Is_Pair(Cdr(expression)) && Cddr(expression) == null_cell,
                    "invalid unquoted expression");
            if (depth == 0)
                unquoted.Push(Cadr(expression));
            else
                Find_Unquoted_Parts(unquoted, Cadr(expression), depth - 1);
        } else if (Car(expression) == quasiquote) {
            Find_Unquoted_Parts(unquoted, Cadr(expression), depth + 1);
        } else {
            Find_Unquoted_Parts(unquoted, Car(expression), depth);
            Find_Unquoted_Parts(unquoted, Cdr(expression), depth);
        }
    } else if (Is_Vector(expression)) {
        size_t size = Vector_Size(expression);
        for (size_t i = 0; i != size; ++i)
            Find_Unquoted_Parts(unquoted, Vector_Ref(expression, i), depth);
    }
}

// Check whether an expression resembles a syntax-rules expression.
bool Is_Syntax_Rules(Cell element) {
    static const Cell syntax_rules = Make_Symbol("syntax-rules");
    return Is_Pair(element) && Is_Identifier(Car(element)) &&
           Extract_Symbol(Car(element)) == syntax_rules;
}

} // namespace uls
