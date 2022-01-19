#pragma once

#include <vector>
#include "noncopyable.hpp"
#include "type.hpp"
#include "cell.hpp"
#include "cell_type.hpp"

namespace uls {

// ,MEMORYMANAGER

// The memory manager. Every interpreter has one of these. They are
// used to allocate cells and they take care of the garbage
// collection.
class Memory_Manager : public noncopyable {
  public:
    Memory_Manager(size_t block_size = 500000);
    ~Memory_Manager();

    static Memory_Manager *mp_;

    // Allocate a cell, you might want to consider using the free
    // function Allocate_Cell if you know exactly how big the cell has
    // to be. This can trigger garbage collection.
    Cell Allocate(size_t size, Cell_Type type, Pointer_Mask mask = max_byte);

    // Discard all cells that are not pointed to by the content of
    // MCells and MStacks
    void Collect_Garbage();

    // Just for debugging, checks whether a cell was missed in the last
    // collection
    bool Is_Valid(Cell cell) {
        return !Is_Compound(cell) || Is_In_Block(cell, _live_block);
    }

    // These are used by MCell and MStack to protect their contents from
    // being collected. You are advised to use those classes instead of
    // using these functions directly.
    static inline void Push_Marked(Cell *cell) { mp_->_marked.push_back(cell); }
    static void Pop_Marked(Cell *cell);
    static inline void Push_Stack(std::vector<Cell> &stack) { mp_->_stacks.push_back(&stack); }
    static inline void Pop_Stack() { mp_->_stacks.pop_back(); }

  private:
    void Move_Cell(Cell *cell);
    bool Is_In_Block(Cell cell, size_t *block) const;
    void Smart_Pop_Marked(Cell *cell);
#ifdef WITH_DESTRUCTORS
    friend class Interpreter;
    void Call_Destructors(size_t old_block_position);
    void Call_All_Destructors();
#endif

    const size_t _block_size;
    size_t _block_position, _cell_header_size;
    size_t *_live_block, *_dead_block;

    std::vector<Cell *> _marked;
    std::vector<std::vector<Cell> *> _stacks;
};

} // namespace uls
