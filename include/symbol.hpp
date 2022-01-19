/* Copyright (c) 2004 Marijn Haverbeke
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any
 * damages arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose, including commercial applications, and to alter it and
 * redistribute it freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must
 *    not claim that you wrote the original software. If you use this
 *    software in a product, an acknowledgment in the product
 *    documentation would be appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must
 *    not be misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 * Marijn Haverbeke
 * marijn(at)haverbeke.nl
 */

#pragma once

#include <string>
#include "cell.hpp"
#include "mcell.hpp"

// Associate integer values with strings. Get_Symbol will create a new
// association if none exist for that string, Find_Symbol will return
// null_symbol in that case. Note that this system never forgets
// associations, this has the disadvantage that it can get memory
// intensive if you generate large amounts of temporary symbols
// (string->symbol and integer->symbol can do that), but the advantage
// that this module is not dependant on any Interpreter and that the
// symbols can be used for other purposes than scheme symbols.

namespace uls {

typedef size_t Symbol;
const Symbol null_symbol = 0;

Symbol Get_Symbol(const std::string &word);
Symbol Find_Symbol(const std::string &word);
const std::string &Get_Symbol_Name(Symbol s);

// First some utility functions to recognize the various kinds of
// chars. Symbol start characters unambiguously mean the rest of the
// element is a symbol.
inline bool Is_Symbol_Start(char c) {
    static const char specials[] = {'!', '$', '%', '&', '*', '/', ':', '<',
                                    '=', '>', '?', '@', '^', '_', '~', 0};
    if (std::isalpha(c))
        return true;
    for (const char *i = specials; *i != 0; ++i) {
        if (c == *i)
            return true;
    }
    return false;
}

// These represent a symbol when they are all by themselves, otherwise
// they are a number.
inline bool Is_Ambiguous_Char(char c) {
    return (c == '+' || c == '-' || c == '.');
}
inline bool Is_Number_Char(char c) {
    return (std::isdigit(c) || Is_Ambiguous_Char(c));
}
// Inside numbers slashes and exponents can appear.
inline bool Is_Internal_Number_Char(char c) {
    return Is_Number_Char(c) || c == 'e' || c == 'E' || c == '/';
}
// Inside symbols anything that is a symbol start or a number can
// appear.
inline bool Is_Symbol_Char(char c) {
    return (Is_Symbol_Start(c) || Is_Number_Char(c));
}

} // namespace uls
