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

#include "inputsplitter.hpp"
#include "error.hpp"
#include "primitive.hpp"
#include "string.hpp"
#include "vector.hpp"
#include "instruction.hpp"

namespace uls {

// for comparing character names
bool String_Compare_Insensitive(const std::string &one,
                                const std::string &two) {
    if (one.size() != two.size())
        return false;
    for (size_t i = 0; i != one.size(); ++i) {
        if (std::tolower(one[i]) != std::tolower(two[i]))
            return false;
    }
    return true;
}

String_Input_Splitter::String_Input_Splitter()
    : _in_string(false), _open_parens(0), _finished_part(0) {}

const std::string &String_Input_Splitter::Current() {
    S_ASSERT(!_parts.empty());
    return _parts.back();
}

void String_Input_Splitter::Advance(bool allow_eof) {
    S_ASSERT(!_parts.empty());
    _parts.pop_back();

    --_finished_part;
    S_ASSERT(_finished_part >= 0);
}

size_t String_End(const std::string &str, size_t string_start) {
    for (size_t i = string_start + 1; i < str.size(); ++i) {
        if (str[i] == '"' && !(i > 0 && str[i - 1] == '\\'))
            return i;
    }
    return str.size();
}

bool Is_Delimiter(char c) {
    return Is_Whitespace(c) || c == '(' || c == ')' || c == ';' || c == '#' ||
           c == '"' || c == '\'' || c == '`' || c == ',';
}

size_t Next_Delimiter(const std::string &str, size_t start) {
    bool escaped = false;
    for (size_t i = start + 1; i < str.size(); ++i) {
        char c = str[i];
        if (!escaped) {
            if (Is_Delimiter(str[i]))
                return i;
            escaped = c == '\\';
        } else {
            escaped = false;
        }
    }
    return str.size();
}

void String_Input_Splitter::Add_Line(const std::string &str) {
    size_t pos = 0;
    if (_in_string) {
        pos = String_End(str, 0);
        if (pos == str.size()) {
            _unfinished = _unfinished + str;
            return;
        } else {
            ++pos;
            Add_Part(_unfinished + std::string(str, 0, pos));
            _in_string = false;
        }
    }
    while (pos < str.size()) {
        char c = str[pos];
        if (Is_Whitespace(c)) {
            ++pos;
        } else if (c == '"') {
            size_t end = String_End(str, pos);
            if (end == str.size()) {
                _unfinished = std::string(str, pos);
                _in_string = true;
                return;
            } else {
                Add_Part(std::string(str, pos, end + 1 - pos));
                pos = end + 1;
            }
        } else if (c == '#' && pos < str.size() && str[pos + 1] == '(') {
            pos += 2;
            ++_open_parens;
            Add_Part("#(");
        } else if (c == '(') {
            ++pos;
            ++_open_parens;
            Add_Part("(");
        } else if (c == ')') {
            ++pos;
            --_open_parens;
            Add_Part(")");
        } else if (c == ',' && pos < str.size() && str[pos + 1] == '@') {
            Add_Part(",@", false);
            pos += 2;
        } else if (c == ',' || c == '`' || c == '\'') {
            Add_Part(std::string(1, c), false);
            ++pos;
        } else if (c == ';') {
            return;
        } else {
            size_t end = Next_Delimiter(str, pos);
            Add_Part(std::string(str, pos, end - pos));
            pos = end;
        }
    }
}

void String_Input_Splitter::Add_Part(const std::string &part,
                                     bool can_be_finished) {
    _parts.push_front(part);
    if (can_be_finished && _open_parens < 1)
        _finished_part = _parts.size();
}

void String_Input_Splitter::Reset() {
    _parts.clear();
    _open_parens = 0;
    _in_string = false;
    _finished_part = 0;
}

bool String_Input_Splitter::Full_Expression() { return _finished_part != 0; }

Stream_Input_Splitter::Stream_Input_Splitter(std::istream &stream)
    : _stream(stream), _lines_read(0), _allow_eof(true) {}

const std::string &Stream_Input_Splitter::Current() {
    if (_current.empty())
        Read_Part();
    return _current;
}

void Stream_Input_Splitter::Advance(bool allow_eof) {
    _current = "";
    _allow_eof = allow_eof;
}

void Stream_Input_Splitter::Read_Part() {
    S_CHECK(_allow_eof || !_stream.eof(),
            "unfinished expression at end of file");

    char c = 0;
    if (!_stream.eof()) {
        c = Get_Char();
        while (Is_Whitespace(c) || c == ';') {
            if (c == ';') {
                while (!_stream.eof() && c != '\n')
                    c = Get_Char();
            } else {
                c = Get_Char();
            }
        }
    }

    if (_stream.eof()) {
        _current = "#\\eof";
    } else if (c == '"') {
        bool escaped = false;
        _current = "\"";
        while (true) {
            c = Get_Char();
            S_CHECK(!_stream.eof(), "unfinished string constant");
            _current += c;
            if (!escaped) {
                if (c == '"')
                    break;
                escaped = c == '\\';
            } else {
                escaped = false;
            }
        }
    } else if (c == ',' && _stream.peek() == '@') {
        Get_Char();
        _current = ",@";
    } else if (c == '#' && _stream.peek() == '(') {
        Get_Char();
        _current = "#(";
    } else if (c == '(' || c == ')' || c == ',' || c == '`' || c == '\'') {
        _current = std::string(1, c);
    } else {
        _current = std::string(1, c);
        bool escaped = false;
        while (true) {
            int next = _stream.peek();
            if (next == std::istream::traits_type::eof())
                break;
            if (!escaped) {
                if (Is_Delimiter(next))
                    break;
                escaped = next == '\\';
            } else {
                escaped = false;
            }

            Get_Char();
            _current += static_cast<char>(next);
        }
    }

    S_ASSERT(!_current.empty());
}


// Convert elements from an Input_Splitter into cells. Reads one whole
// expression. This is a bit of an overgrown monster function... It is
// important that whenever something throws because of invalid input,
// it FIRST advances past that invalid input - otherwise the system
// will go into an infinite loop over that same invalid input.
Cell Input_Splitter::Read(bool allow_eof) {
    const std::string &cur = Current();

    if (cur == "(") { // a list
        Advance(false);
        List_Builder new_list;

        while (true) {
            if (Current() == ")") {
                Advance(allow_eof);
                break;
            } else {
                new_list.Add_Element(Read(false));

                if (Current() == ".") {
                    Advance(false);
                    new_list.Add_End(Read(false));
                    S_CHECK(Current() == ")", "not a valid pair");
                    Advance(false);
                    break;
                }
            }
        }
        return new_list.List();
    } else if (cur[0] == '"') { // a string
        S_ASSERT(cur.size() >= 2);
        std::string value;
        bool escaped = false;
        for (size_t i = 1; i < cur.size() - 1; ++i) {
            if (cur[i] != '\\' || escaped) {
                value += cur[i];
                escaped = false;
            } else {
                escaped = true;
            }
        }
        Advance(allow_eof);
        return Make_String(value);
    } else if (cur == "'") { // a quoted thingy
        Advance(false);
        static const Cell quote = Make_Symbol("quote");
        return Cons(quote, Cons_Null(Read()));
    } else if (cur == "`") { // quasiquoted
        Advance(false);
        static const Cell quasiquote = Make_Symbol("quasiquote");
        return Cons(quasiquote, Cons_Null(Read()));
    } else if (cur == ",") { // unquoted
        Advance(false);
        static const Cell unquote = Make_Symbol("unquote");
        return Cons(unquote, Cons_Null(Read()));
    } else if (cur == ",@") { // splicing-unquoted
        Advance(false);
        static const Cell unquote_splicing = Make_Symbol("unquote-splicing");
        return Cons(unquote_splicing, Cons_Null(Read()));
    } else if (cur == "#(") { // a vector
        Advance(false);
        MStack elements;
        while (Current() != ")")
            elements.Push(Read(false));
        Advance(allow_eof);
        Cell new_vec = Make_Vector(elements);
        return new_vec;
    } else if (cur[0] == '#') {
        if (cur.size() < 2) { // single #
            Advance(allow_eof);
            throw Scheme_Error("invalid input: #");
        }
        char c = cur[1];
        if (cur.size() == 2) { // special
            Advance(allow_eof);
            if (c == 't')
                return true_cell;
            else if (c == 'f')
                return false_cell;
            else if (c == 'v')
                return void_cell;
            else
                throw Scheme_Error(std::string("unrecognized #-expression: #") +
                                   c);
        } else if (c == '\\') { // character
            std::string charname(cur, 2);
            Advance(allow_eof);
            if (charname.size() == 1) {
                return Make_Character(charname[0]);
            } else if (String_Compare_Insensitive(charname, "space")) {
                return Make_Character(' ');
            } else if (String_Compare_Insensitive(charname, "newline")) {
                return Make_Character('\n');
            } else if (String_Compare_Insensitive(charname, "eof")) {
                return eof_cell;
            }
            throw Scheme_Error("unknown character constant: #\\" + charname);
        } else if (c == '%') { // instruction
            std::string ins_name(cur, 2);
            Advance(allow_eof);
            return Make_Instruction(Find_Instruction(ins_name).instruction);
        } else {
            std::string error = "unrecognized #-expression: " + cur;
            Advance();
            throw Scheme_Error(error);
        }
    } else if (Is_Symbol_Start(cur[0]) ||
               (Is_Ambiguous_Char(cur[0]) && cur.size() == 1) ||
               cur == "...") { // symbol
        std::string buffer;
        for (std::string::const_iterator i = cur.begin(); i != cur.end(); ++i) {
            if (!Is_Symbol_Char(*i)) {
                buffer = "invalid character in symbol name: " + cur;
                Advance();
                throw Scheme_Error(buffer);
            }
            buffer += std::tolower(*i);
        }
        Advance(allow_eof);
        return Make_Symbol(buffer);
    } else if (Is_Number_Char(cur[0])) { // number
        std::string saved = cur;
        Advance(allow_eof);
        return String_To_Number(saved);
    } else { // wrong
        std::string error = "unrecognized input: " + cur;
        Advance();
        throw Scheme_Error(error);
    }
}

} // namespace uls
