#pragma once

#include "cell.hpp"
#include "error.hpp"

namespace uls {
// ,STRING

Cell Make_String(const std::string &value);
Cell Make_String(size_t size, char fill = ' ');

std::string String_Value(Cell cell);

struct String_Data {
    size_t size;
    char data[1];
};

inline size_t String_Size(Cell cell) {
    S_ASSERT(Is_String(cell));
    return Extract<String_Data>(cell).size;
}

inline char &String_Ref(Cell cell, size_t n) {
    S_ASSERT(Is_String(cell));
    S_ASSERT(n < String_Size(cell));
    return Extract<String_Data>(cell).data[n];
}

Cell String_To_Number(const std::string &buffer, size_t radix = 10);

void Write_String(Cell cell, std::ostream &str, bool display);

} // namespace uls
