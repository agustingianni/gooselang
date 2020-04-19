# gooselang

Goose complete programming language

- https://people.mpi-sws.org/~rossberg/sml.html
- https://github.com/pascallouisperez/mu-ml
- https://lorgonblog.wordpress.com/2008/04/09/catamorphisms-part-three/
- https://github.com/dotnet/corert
- https://www.beeflang.org/
- https://eev.ee/blog/2016/12/01/lets-stop-copying-c/
- http://www.stephendiehl.com/llvm/
- https://ziglang.org/documentation/master/
- https://github.com/fscheck/FsCheck
- https://github.com/vain0x/milone-lang.git
- https://github.com/FrankBro/ordo
- https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora058.html

## Compiler Phases

- Lexing
  - Generates a token list
- Parsing
  - Generates an AST
- Import Resolution
  - Imports all the types from modules
- Type Checking
  - Iteratively typechecks the AST
  - Constraint resolution
  - Type Inference
  - Generates a Typed AST
- Code Generation
  - Lowering
  - Optimization
- IL emit
  - Emit IL
  - Binary creation

## Bindings

```fsharp
let name = 1
```

## Modules

```fsharp
let MyModule = module {
    let x = 1
}
```

## Types

```fsharp
let myStruct = struct {
    name : type
    name : type
}

let myUnion = union {
    name : type
    name : type
}
```

## Functions

```fsharp
let inc = lambda (a) {
    a + 1
}

let sum = lambda (a, b) {
    a + b
}

let inc = lambda (a:int):int {
    a + 1
}

let sum = lambda (a:int, b:int):int {
    a + b
}

```

## Array

```fsharp
let array = array { 1 ; 2 ; 3 }

let array = array {
    1
    2
    3
}
```

## Tuple

```fsharp
let tuple = tuple { 1 ; "a" ; ident }

let tuple = tuple {
    1
    "a"
    ident
}
```

## Dictionary

```fsharp
let dictionary = dict { 1 : "one" ; 2 : "two" }

let dictionary = dict {
    1 : "one"
    2 : "two"
}
```

## Predicates

```fsharp
empty ? list
```

## Symbols

```fsharp
s"HOLA"
```

## Iteration

```fsharp
for x in things {
    printf x
}
```

```fsharp
repeat 1000
    printf "Hola!"

```

## In / Out

```fsharp
open IO

printf "hola Agustin!"
let variable = "Agustin"
printf f"hola {variable}!"
```

## Standard Library

### String

The string type represents immutable text as a sequence of Unicode characters.

#### Methods

- concat
- join
- indexOf
- contains
- startsWith
- endsWith
- remove
- replace
- split
- substring
- trim
- trimStart
- trimEnd
- equals
- toLower
- toUpper
- padLeft
- padRight
- length
- find

#### Indexing

```fsharp
let s = "012345678"
s[0] == "0"
s[0 .. 2] = "012"
s[0 .. 2 .. 8] = "02468"
```

### Array

### List

#### Methods

- fold
- map

### Set

## Others

LINQ like DSL for provided types like arrays, sets etc.
