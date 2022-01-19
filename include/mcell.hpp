#pragma once

#include "cell.hpp"
#include "memory_manager.hpp"

namespace uls {

// MCell is used to contain one cell (objects of this class can be
// implicitly converted from and to cells) and protect it from being
// garbage collected.
class MCell {
  public:
    MCell(Cell cell = null_cell);
    MCell(const MCell &other);
    ~MCell();

    inline operator Cell &() { return _cell; }
    inline operator Cell() const { return _cell; }
    inline void operator=(Cell cell) { _cell = cell; }
    inline void operator=(const MCell &mcell) { _cell = mcell._cell; }

  private:
    Cell _cell;
};

// Convenient ways of allocating a cell. Use the template argument of
// Allocate_Cell to specify what kind of data you want to store in the
// cell (and then get access to that with Extract<Data> after it has
// been allocated).
Cell Allocate(size_t size, Cell_Type type, Pointer_Mask mask = max_byte);

template <class Data>
inline Cell Allocate_Cell(Cell_Type type, Pointer_Mask mask = max_byte) {
    return Allocate(sizeof(Data), type, mask);
}

Cell Make_Rational(const MCell &numerator, const MCell &denominator);

// MStack is like MCell but instead it protects a whole stack of
// cells. Has a std::vector-like interface. You must not allocate
// these as function statics or on the heap, they rely on being
// destructed in the same order they were created.
class MStack {
  public:
    MStack();
    explicit MStack(size_t size);
    ~MStack();

    Cell &operator[](size_t n) { return _cells[n]; }
    Cell operator[](size_t n) const { return _cells[n]; }
    void Push(Cell cell) { _cells.push_back(cell); }
    Cell Pop() {
        Cell temp = _cells.back();
        _cells.pop_back();
        return temp;
    }
    bool Empty() const { return _cells.empty(); }
    size_t Size() const { return _cells.size(); }
    Cell &Back() { return _cells.back(); }
    void Clear() { _cells.clear(); }

  private:
    std::vector<Cell> _cells;
};

} // namespace uls
