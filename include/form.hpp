#pragma once

#include "cell.hpp"

namespace uls {

// ,SPECIAL FORM

enum Form_Name {
    form_if,
    form_define,
    form_set,
    form_quote,
    form_quasiquote,
    form_lambda,
    form_begin,
    form_define_macro,
    form_define_syntax,
    form_let_syntax,
    form_letrec_syntax,
    form_current_env
    // Current_env is a hack to make interaction-environment possible
    // (the virtual machine itself does not know much about
    // environments, so it has to be fetched at compile time, for which
    // a special form is needed.)
};

inline Cell Make_Special_Form(Form_Name name) {
    return Encode_Sixbit(name, form_pattern);
}
inline Form_Name Special_Form_Name(Cell cell) {
    size_t temp = Extract_Sixbit(cell);
    return reinterpret_cast<Form_Name &>(temp);
}

} // namespace uls
