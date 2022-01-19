#pragma once

#include "memory_manager.hpp"
#include "mcell.hpp"
#include "cell_type.hpp"
#include "inputsplitter.hpp"

namespace uls {

// New types can be created with the Make_Type function. It is up to
// the user code to keep track of this value and pass it to
// Allocate_Cell when creating cells of this type. The write function
// will be invoked when a cell of that type is written, displayed or
// converted to a string in some other way. The third argument
// indicates whether display or write was used, currently only strings
// and characters write differently when display is true.

typedef void (*Write_Function)(Cell cell, std::ostream &str, bool display);

#ifdef WITH_DESTRUCTORS
typedef void (*Destroy_Function)(Cell cell);
Cell_Type Make_Type(Write_Function write, Destroy_Function destroy = NULL);
#else
Cell_Type Make_Type(Write_Function write);
#endif

// ,TYPE MANAGER

// Associates write functions with cell types. Just use Make_Type and
// ignore this class.
class Type_Manager : public noncopyable {
public:
    Type_Manager();
#ifdef WITH_DESTRUCTORS
    Cell_Type Make_Type(Write_Function write, Destroy_Function destroy);
#else
    Cell_Type Make_Type(Write_Function write);
#endif
    inline Write_Function Get_Function(Cell_Type type) {
        S_ASSERT(type < _functions.size());
        S_ASSERT(_functions[type] != NULL);
        return _functions[type];
    }
#ifdef WITH_DESTRUCTORS
    Destroy_Function Get_Destructor(Cell_Type type) {
        S_ASSERT(type < _destructors.size());
        return _destructors[type];
    }
#endif

private:
    std::vector<Write_Function> _functions;
#ifdef WITH_DESTRUCTORS
    std::vector<Destroy_Function> _destructors;
#endif
    Cell_Type _current;
};

// ,INTERPRETER

// This is what you create an instance of to start working with
// scheme. Everything is public, and the only member function is the
// constructor.
//
// The argument to the constructor gives the amount of kilobytes a
// memory block must contain. The memory allocated by the mem manager
// is twice this, because of the garbage collection method used.
//
// Messing with the variables in this struct should be rather safe.
// You can call functions on the mem_manager and type_manager if you
// must, change the standard input and output, messing with the
// environments is probably a bad idea.
class Interpreter {
public:
    explicit Interpreter(size_t memory = 2000);
    ~Interpreter();

    static Interpreter* ip_;

    static inline Type_Manager& TypeManager() { return ip_->type_manager; }
    static inline Memory_Manager& MemoryManager() { return ip_->mem_manager; }
    static inline MCell& Input() { return ip_->input; }
    static inline MCell& Output() { return ip_->output; }
    static inline MCell& NullEnv() { return ip_->null_env; }
    static inline MCell& ReportEnv() { return ip_->report_env; }
    static inline MCell& WorkEnv() { return ip_->work_env; }

    static void SetOutput(MCell newOutput) { ip_->output = newOutput; }

    static inline Write_Function Get_Function(Cell_Type type) {
        return Interpreter::ip_->type_manager.Get_Function(type);
    }

    void Run_REPL(bool welcome_message);

private:
    Type_Manager type_manager;
    Memory_Manager mem_manager;

    MCell null_env;
    MCell report_env;
    MCell work_env;
    MCell input;
    MCell output;
};

// Evaluate a string or an expression. Only the first expression in
// the given string is evaluated.
Cell Eval_String(const std::string &str, const MCell &name_space,
                 bool handle_errors = false);
Cell Eval_String(const std::string &str, bool handle_errors = false);

Cell Eval_Expression(Cell expression, Cell name_space,
                     bool handle_errors = false);
Cell Eval_Expression(Cell expression, bool handle_errors = false);

// Get the value that a binding has in a namespace. Looks through
// parent namespaces too.
Cell Get_Value(Cell name_space, Cell symbol);

void Load_File(const std::string &filename);

// Starts a read-eval-print loop. This will not return until in.input
// is at eof.
void Run_REPL(bool welcome_message = true);
// Loads a file (just executes the load function defined in init.scm
// with the file as argument)
void Load_File(const std::string &filename);

// A read eval print loop that does not wait for input to come from a
// stream but has to be fed strings to run. The return value of
// Add_Line indicates whether anything got evaluated (if the new
// string did not finish a full expression it is false).
class String_REPL {
  public:
    bool Add_Line(const std::string &str);

  private:
    String_Input_Splitter _input;
};

// This is useful if you want to poll the output instead of have it go
// into a stream, basically just attaches itself to in.output and
// gives you any new output every time you call Get_New_Output. Makes
// the assumption that no one else messes with in.output while it is
// alive.
class Output_Catcher {
  public:
    Output_Catcher();
    ~Output_Catcher();
    std::string Get_New_Output();

  private:
    std::ostringstream _stream;
    MCell _old_stream;
};

std::string Find_Init_File();

Cell Syntax_Rules_To_Macro(Cell syntax_rules, Cell name,
                           const MCell &environment, const MCell &name_space);

Cell Adjust_Arguments(Cell argument_vector, int num_values, int num_args,
                      Cell function_name);

void Grow_Environment(const MCell &environment, size_t new_size);

void Find_Unquoted_Parts(MStack &unquoted, Cell expression, size_t depth);

bool Is_Syntax_Rules(Cell element);
} // namespace uls
