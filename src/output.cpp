#include <fstream>
#include "output.hpp"
#include "inputsplitter.hpp"
#include "string.hpp"
#include "cell.hpp"
#include "primitive.hpp"
#include "bignum.hpp"
#include "number_io.hpp"

namespace uls {

// ,PORT

void Write_Inport(Cell cell, std::ostream &str, bool display) {
    Inport_Data &data = Extract<Inport_Data>(cell);
    str << "#<inport";
    if (data.file_name != false_cell)
        str << ':' << String_Value(data.file_name);
    str << '>';
}
void Write_Outport(Cell cell, std::ostream &str, bool display) {
    Outport_Data &data = Extract<Outport_Data>(cell);
    str << "#<outport";
    if (data.file_name != false_cell)
        str << ':' << String_Value(data.file_name);
    str << '>';
}

// File ports keep track of their filename.
Cell Make_Inport(const MCell &filename) {
    Cell new_port = Allocate_Cell<Inport_Data>(inport_type, (std::byte) 1);
    Inport_Data &data = Extract<Inport_Data>(new_port);
    std::ifstream *new_stream =
        new std::ifstream(String_Value(filename).c_str(), std::ios::binary);
    S_CHECK(!new_stream->fail(),
            "could not open file " + String_Value(filename));
    data.file_name = filename;
    data.stream = new_stream;
    data.position = 0;
    data.line = 1;
    return new_port;
}
// Reopening file ports is a useful trick. It works as follows -
// whenever a file gets closed it records its position (for input
// files, output files can just start appending), it can then be
// reopened to start at that position. This combines very well with
// dynamic-wind, it allows code inside the wind to read or write front
// to back without worrying about it getting closed an reopened.
void Reopen_Inport(Cell port) {
    S_ASSERT(Is_Inport(port));
    Inport_Data &data = Extract<Inport_Data>(port);
    S_CHECK(data.file_name != false_cell, "only file ports can be reopened");
    if (data.stream == nullptr) {
        std::ifstream *new_stream = new std::ifstream(
            String_Value(data.file_name).c_str(), std::ios::binary);
        S_CHECK(!new_stream->fail(),
                "could not open file " + String_Value(data.file_name));
        new_stream->seekg(data.position);
        if (new_stream->fail())
            new_stream->setstate(std::ios_base::eofbit);
        data.stream = new_stream;
    }
}
Cell Make_Outport(const MCell &filename) {
    Cell new_port = Allocate_Cell<Outport_Data>(outport_type, (std::byte) 1);
    Outport_Data &data = Extract<Outport_Data>(new_port);
    std::ofstream *new_stream =
        new std::ofstream(String_Value(filename).c_str(), std::ios::binary);
    S_CHECK(!new_stream->fail(),
            "could not open file " + String_Value(filename));
    data.file_name = filename;
    data.stream = new_stream;
    return new_port;
}
void Reopen_Outport(Cell port) {
    S_ASSERT(Is_Outport(port));
    Outport_Data &data = Extract<Outport_Data>(port);
    S_CHECK(data.file_name != false_cell, "only file ports can be reopened");
    if (data.stream == nullptr) {
        std::ofstream *new_stream =
            new std::ofstream(String_Value(data.file_name).c_str(),
                              std::ios::binary | std::ios::app);
        S_CHECK(!new_stream->fail(),
                "could not open file " + String_Value(data.file_name));
        data.stream = new_stream;
    }
}

// Ports based on existing streams. The filename field is #f for
// these, closing or reopening them is a no-op.
Cell Make_Inport(std::istream &stream) {
    Cell new_port = Allocate_Cell<Inport_Data>(inport_type, (std::byte) 1);
    Inport_Data &data = Extract<Inport_Data>(new_port);
    data.file_name = false_cell;
    data.stream = &stream;
    data.position = 0;
    data.line = 1;
    return new_port;
}
Cell Make_Outport(std::ostream &stream) {
    Cell new_port = Allocate_Cell<Outport_Data>(outport_type, (std::byte) 1);
    Outport_Data &data = Extract<Outport_Data>(new_port);
    data.file_name = false_cell;
    data.stream = &stream;
    return new_port;
}

Cell Inport_Read_Char(Cell port) {
    S_ASSERT(Is_Inport(port));
    Inport_Data &data = Extract<Inport_Data>(port);
    if (data.stream == nullptr)
        return eof_cell;
    char c = data.stream->get();
    if (c == '\n')
        ++data.line;

    if (data.stream->eof())
        return eof_cell;
    else
        return Make_Character(c);
}
Cell Inport_Peek_Char(Cell port) {
    S_ASSERT(Is_Inport(port));
    Inport_Data &data = Extract<Inport_Data>(port);
    if (data.stream == nullptr)
        return eof_cell;
    char c = data.stream->peek();
    if (data.stream->eof())
        return eof_cell;
    else
        return Make_Character(c);
}

std::istream &Inport_Stream(Cell port) {
    S_ASSERT(Is_Inport(port));
    Inport_Data &data = Extract<Inport_Data>(port);
    S_CHECK(data.stream != nullptr, "taking input stream from a closed file");
    return *data.stream;
}
// Current line of the input port.
size_t Inport_Line(Cell port) {
    S_ASSERT(Is_Inport(port));
    return Extract<Inport_Data>(port).line;
}

std::ostream &Outport_Stream(Cell cell) {
    S_ASSERT(Is_Outport(cell));
    std::ostream *pointer = Extract<Outport_Data>(cell).stream;
    S_CHECK(pointer != nullptr, "taking output stream from a closed file");
    return *pointer;
}

// This might work on some implementations, it does not work on GCC.
// Used to implement char-ready?, which is kind of hard to do with the
// blocking streams of the standard library.
bool Inport_Ready(Cell port) {
    S_ASSERT(Is_Inport(port));
    std::istream *stream = Extract<Inport_Data>(port).stream;
    return stream != nullptr && stream->rdbuf()->in_avail() > 0;
}

// Closing already closed ports or non-file ports does not do
// anything.
void Close_Inport(Cell cell) {
    S_ASSERT(Is_Inport(cell));
    Inport_Data &data = Extract<Inport_Data>(cell);
    if (data.file_name != false_cell && data.stream != nullptr) {
        data.position = data.stream->tellg();
        static_cast<std::ifstream *>(data.stream)->close();
        delete data.stream;
        data.stream = nullptr;
    }
}
void Close_Outport(Cell cell) {
    S_ASSERT(Is_Outport(cell));
    Outport_Data &data = Extract<Outport_Data>(cell);
    if (data.file_name != false_cell && data.stream != nullptr) {
        static_cast<std::ofstream *>(data.stream)->close();
        delete data.stream;
        data.stream = nullptr;
    }
}

Cell Read(std::istream &stream) {
    Stream_Input_Splitter splitter(stream);
    return splitter.Read();
}

// ,PAIR

// Actually writes a whole list.
void Write_Pair(Cell cell, std::ostream &str, bool display) {
    str << "(";
    while (true) {
        Write(Car(cell), str, display);
        if (Cdr(cell) == null_cell)
            break;
        if (!Is_Pair(Cdr(cell))) {
            str << " . ";
            Write(Cdr(cell), str, display);
            break;
        }
        str << ' ';
        cell = Cdr(cell);
    }
    str << ')';
}


void Write_Vector(Cell cell, std::ostream &str, bool display) {
    str << "#(";
    size_t size = Vector_Size(cell);
    for (size_t i = 0; i < size; ++i) {
        Write(Vector_Ref(cell, i), str, display);
        if (i != size - 1)
            str << ' ';
    }
    str << ')';
}


void Write_Bignum(Cell cell, std::ostream &str, bool display) {
    Bignum_Data &data = Get_Bignum_Data(cell);
    Write_Array(str, data.data, data.size, data.negative);
}

} // namespace uls
