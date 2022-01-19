#include "memory_manager.hpp"
#include "interpreter.hpp"

namespace uls {

Memory_Manager *Memory_Manager::mp_ = nullptr;

// ,MEMMANAGER

// When a cell is moved during garbage collection its new location is
// stored in the first data field of the old location.
inline Cell &Reference_To_Moved(Cell cell) {
    return Compound_Info(cell).data[0];
}

// The method used for garbage collection is stop-and-copy. There are
// two blocks of equal size, only one is in use. When the block is
// full all 'live' cells get copied to the other block and the blocks
// switch roles. Allocation is simply a matter of putting the new cell
// at the current position in the live block and incrementing that
// position by the cell size. Big disadvantage of this method is that
// it moves stuff around when collecting.
Memory_Manager::Memory_Manager(size_t block_size)
    : _block_size(block_size), _block_position(0),
      _cell_header_size(Fit_To_Four(sizeof(Cell_Info) - sizeof(Cell))),
      _live_block(new size_t[block_size]), _dead_block(new size_t[block_size]) {
    mp_ = this;
}

Memory_Manager::~Memory_Manager() {
    delete[] _live_block;
    delete[] _dead_block;
    mp_ = nullptr;
}

// Allocate a new cell, collect garbage if needed.
Cell Memory_Manager::Allocate(size_t size, Cell_Type type, Pointer_Mask mask) {
    size = Fit_To_Four(size) + _cell_header_size;
    if (size == _cell_header_size)
        size += 1;

    // Check if there is room, collect otherwise.
    if (size > (_block_size - _block_position)) {
        Collect_Garbage();
        S_CHECK(size <= (_block_size - _block_position), "out of memory");
    }

    // Always collect garbage. this makes some easy-to-make errors
    // manifest themselves. Awfully slow though.
#ifdef ALWAYS_COLLECT
    Collect_Garbage();
#endif

    size_t *new_cell = _live_block + _block_position;
    _block_position += size;

    // Fill in the Cell_Info struct for the cell.
    Cell cell = (Cell)new_cell;
    Cell_Info &info = Compound_Info(cell);
    info.size = size;
    info.mask = mask;
    info.type = type;

    return cell;
}

void Memory_Manager::Collect_Garbage() {
#ifdef WITH_DESTRUCTORS
    size_t old_block_position = _block_position;
#endif
    // switch the dead and the live blocks
    _block_position = 0;
    std::swap(_live_block, _dead_block);

    // move all reachable cells over to the new live block

    // Put the cells in that are marked by MCell or MStack objects in a
    // stack. They are the start from which all reachable cells are
    // reached.
    std::vector<Cell *> stack(_marked);
    for (std::vector<std::vector<Cell> *>::iterator i = _stacks.begin();
         i != _stacks.end(); ++i) {
        for (std::vector<Cell>::iterator j = (*i)->begin(); j != (*i)->end();
             ++j)
            stack.push_back(&(*j));
    }

    // Handle every cell on the stack. Handling a cell may cause other
    // cells to be pushed onto it.
    while (!stack.empty()) {
        Cell *current = stack.back();
        stack.pop_back();

        // first check current pointer - if it is not a compound cell
        // it is not a pointer, if it already points to the live block it
        // is already up to date
        if (Is_Compound(*current) && Is_In_Block(*current, _dead_block)) {
            if (Compound_Info(*current).type ==
                moved_cell_type) { // means the cell has already been moved
                *current = Reference_To_Moved(*current);
            } else {
                Move_Cell(current);
                Cell_Info &info = Compound_Info(*current);

                Pointer_Mask mask = info.mask;
                size_t words = info.size - _cell_header_size;

                // if mask is all 1's all fields get pushed
                if (mask == max_byte) {
                    for (size_t i = 0; i < words; ++i)
                        stack.push_back(info.data + i);
                }
                // else if mask contains at least some 1's the fields that are
                // indicated get pushed.
                else if (mask != (std::byte) 0) {
                    for (size_t i = 0; i < words && (mask >> i) != (std::byte) 0; ++i) {
                        if (((mask >> i) & (std::byte) 1) != (std::byte) 0)
                            stack.push_back(info.data + i);
                    }
                }
            }
        }
    }

#ifdef WITH_DESTRUCTORS
    Call_Destructors(old_block_position);
#endif
}

// Moves a cell, sets a pointer to the new location at the old
// location and sets the size of the old location to 0 to indicate the
// cell has been moved.
void Memory_Manager::Move_Cell(Cell *cell) {
    Cell_Info &info = Compound_Info(*cell);

    size_t *old_location = reinterpret_cast<uintptr_t *>(*cell);
    size_t *new_location = _live_block + _block_position;
    _block_position += info.size;

    std::copy(old_location, old_location + info.size, new_location);

    info.type = moved_cell_type;
    *cell = info.data[0] = (Cell)new_location;
}

// Check whether a cell is in a block
bool Memory_Manager::Is_In_Block(Cell cell, size_t *block) const {
    size_t *b_cell = (size_t *)cell;
    return b_cell >= block && b_cell < block + _block_size;
}

// Remove a marked cell if it is not on top of the stack.
void Memory_Manager::Smart_Pop_Marked(Cell *cell) {
    size_t new_size = _marked.size() - 1;
    size_t position = new_size - 1;
    while (_marked[position] != cell) {
        S_ASSERT(position != 0);
        --position;
    }
    for (; position != new_size; ++position)
        _marked[position] = _marked[position + 1];
    _marked.pop_back();
}

#ifdef WITH_DESTRUCTORS
void Memory_Manager::Call_Destructors(size_t old_block_position) {
    size_t *point = _dead_block;
    size_t *end = point + old_block_position;

    while (point < end) {
        Cell current = (Cell)point;
        Cell_Info &info = Compound_Info(current);
        if (info.type != moved_cell_type) {
            Destroy_Function destructor =
                Interpreter::ip_->type_manager.Get_Destructor(info.type);
            if (destructor != nullptr)
                destructor(current);
        }
        point += info.size;
    }
}

void Memory_Manager::Call_All_Destructors() {
    std::swap(_live_block, _dead_block);
    Call_Destructors(_block_position);
}
#endif

void Memory_Manager::Pop_Marked(Cell *cell) {
    if (mp_->_marked.back() == cell)
        mp_->_marked.pop_back();
    else
        mp_->Smart_Pop_Marked(cell);
}

} // namespace uls
