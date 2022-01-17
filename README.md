# Unlikely Scheme

by Marijn Haverbeke

README (for version .45, October 7th 2004)

## Introduction

Unlikely Scheme is a small scheme interpreter. It is more or less R5RS
compliant, although I am sure it deviates on some obscure points that
I am unaware of. Still, most 'standard' scheme code should run on it.

The thing this program is supposed to do better than all the other
scheme implementations is embedding in C++ applications. I wrote it to
provide scheme scripting capability for a graphical application, and
some care was taken to make it fit nicely in an environment other than
a stdin/stdout console. The overall C++ interface is quite clean I
think, it makes use of the conveniences C++ offers (destructors,
exceptions, overloading, etc). Exceptions are used to signal errors,
so no scary longjmp-ing happens. A part of the interface that I'm
still not happy with is the 'protecting' of scheme data from the
garbage collector. I used a moving garbage collection method, so apart
from having to protect data from being collected, any pointer-like
data can become invalid when memory is allocated unless you protect
it. This is not very convenient, any better solutions are welcome.

The performance of the interpreter is 'not bad'. It is (heavily
depending on the kind of benchmark you pick) about a factor 3 slower
than spiffy implementations like mzscheme or scheme48. At any rate
this is a work in progress, if you have any useful contributions I'd
love to hear about them.

## License

This software is released under the zlib license. Basically you can
use/copy/modify it however you like as long as you do not claim you
wrote it and keep the license notice with it. See COPYING.

## Installation

I have (hopefully more or less up-to-date) MS Windows binaries hosted
at the same place as the source. For unix systems compiling with the
makefile that comes with the source should not be too hard. It needs
GNU make, g++ and unix stuff to work though. On windows this can be
done with cygwin and/or mingw. To build the binary like this you
simply type 'make install' (or maybe gmake). This will build the
binary (uscheme) and the libary. By default they are installed to
/usr/local, with the header files in /usr/local/include/uscheme. To
build a debug version use 'make TYPE=fast-debug'. With 'TYPE=debug'
you get a debug version that collects garbage on every memory
allocation. This is great for finding memory errors, but it is
_really_ slow.

I have not tried to compile on a lot of compilers yet but as far as I
know I use standard C++ everywhere. Creating a project on your
favorite IDE should not be hard. Compiling all the cpp files together
creates the stand alone interpreter, and for the library you leave out
main.cpp.

## Programming Interface

This is only a short summary because I am too lazy to write a good
documentation for a beta whose interface will probably change anyway.
But the headers (<uscheme/scheme.hpp> if you installed with the
makefile) are well commented and quite simple.

Initializing an interpreter happens by creating an object of the type
Interpreter. You can only have one of these alive at a time. You can
then load in scheme files with
```
void Load_File(const std::string& filename)
```
And to evaluate a string use
```
Cell Eval_String(const std::string& str, bool handle_errors)
```
The second argument indicates whether exceptions should be let out or
handled by the interpreter. The return value is the result of
evaluating the string.

That should allow you to write a trivial application. For more useful
stuff you'll have to know that the basic scheme data objects have the
type Cell in C++. Small objects are stuffed into a single 32-bit value
(symbols, characters and small integers mostly), while for bigger
objects this value is a pointer to memory where the object lives.
These bigger objects are where the garbage collection trouble comes
in. Whenever anything gets allocated (or garbage gets collected
explicitly) stuff can move, and if you have pointer data in a raw Cell
you can consider it invalid. To help this problem there is another
type, MCell, which encapsulates a cell and makes sure that it does not
get collected and that the pointer gets updated when it is moved.

The most common way of extending scheme in C++ is by adding
primitives. To make a primitive you make a function that
* returns a Cell
* has 0..8 Cells as arguments

You can then make a binding for it in the default top-level
environment with:
```
template <typename Function_Type>
void Define_Primitive(const std::string& name,
                      Function_Type function, bool var_arg)
```
`var_arg` indicates whether the last argument matches to one argument or
the list of remaining arguments. The number of arguments is
automatically deduced from the type of the function pointer.

It is also possible to define new types, but for now you'll have to
figure out how to do that yourself. (Hint: look at Make_Type and look
how pairs and vectors and such are implemented). Scheme.hpp is full of
other functions that you would expect in a scheme implementation (Car,
Cons, Is_Number, and so on). I chose not to declare the functions that
implement the primitives in the header file, since any of them that do
anything generally useful are implemented in terms of another function
that IS declared in the header.

Marijn Haverbeke
marijn(at)haverbeke.nl
