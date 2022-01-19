#include "bignum.hpp"
#include "cell.hpp"
#include "mcell.hpp"
#include "number_io.hpp"
#include "string.hpp"

namespace uls {

Cell Make_String(size_t size, char fill) {
    Cell new_string = Allocate(sizeof(size_t) + size, string_type, (std::byte) 0);
    String_Data &data = Extract<String_Data>(new_string);
    data.size = size;
    for (size_t i = 0; i != size; ++i)
        data.data[i] = fill;
    return new_string;
}

Cell Make_String(const std::string &value) {
    // Allocates a cell of a size dependant on the string size.
    Cell new_string = Allocate(sizeof(size_t) + value.size(), string_type, (std::byte) 0);
    String_Data &data = Extract<String_Data>(new_string);
    data.size = value.size();
    std::copy(value.begin(), value.end(), data.data);
    return new_string;
}

std::string String_Value(Cell cell) {
    S_ASSERT(Is_String(cell));
    size_t size = String_Size(cell);
    if (size == 0)
        return "";
    else
        return std::string(&String_Ref(cell, 0), size);
}

// Convert a string to the correct kind of number.
Cell String_To_Number(const std::string &buffer, size_t radix) {
    size_t slash = buffer.find('/');
    // When radix is more than 14 e means 14 so only E can be used for
    // exponent.
    size_t dot_or_exp =
        (radix > 14) ? buffer.find_first_of(".E") : buffer.find_first_of(".eE");
    if (slash != buffer.npos) {
        MCell numerator =
                  String_To_Number(std::string(buffer, 0, slash), radix),
              denominator =
                  String_To_Number(std::string(buffer, slash + 1), radix);
        S_CHECK(Is_Integer(numerator) && Is_Integer(denominator),
                "invalid rational constant: " + buffer);
        return Make_Simplified_Rational(numerator, denominator);
    } else if (dot_or_exp != buffer.npos) {
        return Make_Real(String_To_Double(buffer, radix));
    } else { // integer
        // Read the integer as a bignum when it is bigger than 19 decimal
        // digits, otherwise read it (more efficiently) as an int64. When
        // the radix is not 10 we always use the bignum version, because
        // it got too complicated to calculate when those numbers were too
        // big to fit in an int64.
        if (buffer.size() > 19 || radix != 10) {
            std::vector<digit> array;
            bool negative = String_To_Array(buffer, array, radix);
            return Make_Integer(&array[0], array.size(), negative);
        } else {
            return Make_Integer(String_To_Int(buffer, radix));
        }
    }
}

void Write_String(Cell cell, std::ostream &str, bool display) {
    if (!display)
        str << '"';
    size_t size = String_Size(cell);
    for (size_t i = 0; i < size; ++i) {
        char c = String_Ref(cell, i);
        if (!display && (c == '\\' || c == '"'))
            str << '\\';
        str << c;
    }
    if (!display)
        str << '"';
}

} // namespace uls
