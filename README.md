# python_interpreter
This is a python interpreter made in Haskell. It supports a few features of original Python, although it's also powerful. It includes the most of native python's datatypes, as int, list, float, string and bool. It also has grown to support unary and binary operations, such as `1 + 1` and the support to more number of arguments is totally full now, but it doesn't follow yet the original convention of Python (AKA PEP) of limited number of arguments but it will be done soon. It also supports lists/string concatenation via the same operator. The support of bidimensional lists are also specified. The lexer is able to know whether a variable is bounded to either local or global scope. It also has control structures such as `for` and `while`. The parser is able to identify a class, methods, functions and nested structures, and soon it will also support duck typing. It supports if/else and its ternary version. Lambda functions were not forgotten, it was added up into the interpreter.

# Where it differs from python?

It cannot interpret a file for now, but it will be done in the future. It can run about 57x faster than original Python, and it has no GIL, it also supports comments via `//` and `/*` but will add support to both `#` and `//` syntax. And to enforce metaprogramming, I'll be adding support to dinamically addition of methods.

# TODO
- more support to reflexive programming via `dir()` and `type()`
- add optional typing instead of type hints
- division operator
- support to infix operators
- support to read a file
- IO functions
- dictionaries
- support for duck typing and dynamic dispatching
- support for comments with `#` and `"""`
- support for `try` and `raise` functions
- support for import and no need to declare a `__init__.py`

# How to run?

Make sure you have stack installed in your PC, then run:

`$ stack run`
