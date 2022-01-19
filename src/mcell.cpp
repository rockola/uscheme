#include "cell.hpp"
#include "mcell.hpp"

namespace uls {

Cell Make_Rational(const MCell &numerator, const MCell &denominator) {
    S_ASSERT(denominator != zero_cell); // caller should make sure
    Cell retval = Allocate_Cell<Rational_Data>(rational_type);
    Rational_Numerator(retval) = numerator;
    Rational_Denominator(retval) = denominator;
    return retval;
}

MCell::MCell(Cell cell) : _cell(cell) { Memory_Manager::mp_->Push_Marked(&_cell); }

MCell::MCell(const MCell &other) : _cell(other._cell) {
    Memory_Manager::Push_Marked(&_cell);
}

MCell::~MCell() { Memory_Manager::mp_->Pop_Marked(&_cell); }

Cell Allocate(size_t size, Cell_Type type, Pointer_Mask mask) {
    return Memory_Manager::mp_->Allocate(size, type, mask);
}

MStack::MStack() { Memory_Manager::mp_->Push_Stack(_cells); }

MStack::MStack(size_t size) : _cells(size, null_cell) {
    Memory_Manager::mp_->Push_Stack(_cells);
}

MStack::~MStack() { Memory_Manager::mp_->Pop_Stack(); }

} // namespace uls
