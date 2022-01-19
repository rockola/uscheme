#include "syntax.hpp"
#include "mcell.hpp"
#include "form.hpp"

namespace uls {

// ,INITIALIZATION

#define FORM(name, value)                                                      \
    Define_Symbol(name_space, name, Make_Special_Form(value))

void Initialize_Syntax(const MCell &name_space) {
    FORM("if", form_if);
    FORM("define", form_define);
    FORM("set!", form_set);
    FORM("quote", form_quote);
    FORM("quasiquote", form_quasiquote);
    FORM("begin", form_begin);
    FORM("lambda", form_lambda);
    FORM("let-syntax", form_let_syntax);
    FORM("letrec-syntax", form_letrec_syntax);
    FORM("define-macro", form_define_macro);
    FORM("define-syntax", form_define_syntax);
    FORM("impl:current-env", form_current_env);
}

#undef FORM

} // namespace uls
