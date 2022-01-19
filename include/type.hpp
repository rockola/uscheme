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

#include <cstddef>
#include <cstdint>

// Some typedefs and constant to make some bit-twiddling operations
// and typenames a little less ugly.

namespace uls {

typedef std::byte byte;
typedef int64_t int64;
typedef uint64_t uint64;
static constexpr size_t byte_size = 8;
static constexpr std::byte max_byte = (std::byte)0xFF;
static constexpr std::uint16_t max_short = 0xFFFF;
// TODO 64-bit fixnums
static constexpr size_t max_int = 0xFFFFFFFF;
// Fixnums range from -max_fixnum to +max_fixnum
static constexpr uintptr_t max_fixnum = (max_int >> 2);

} // namespace uls
