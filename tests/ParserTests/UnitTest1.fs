// https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-fsharp-with-nunit
module ParserTests

open NUnit.Framework
open FSharp.Text.Lexing

open AST

let Parse input =
    let tokenized = LexBuffer<char>.FromString(input)
    try
        let tt a =
            // eprintf "%A" a
            Lexer.tokenize a

        Parser.program tt tokenized
    with e ->
        let pos = tokenized.EndPos
        let line = pos.Line
        let column = pos.Column
        let lastToken = System.String(tokenized.Lexeme)
        eprintfn "Parse failed at line %d, column %d" line column
        eprintfn "Input: %A" input
        exit 1

let getProgramExpression = function
    | Program(_, expr) -> expr

[<Test>]
let TestNumbers() =
    // Program ([],Constant (Uint8 231uy))
    // let numbers_uint16 = lazy [0us .. 0xffffus]
    // let numbers_uint32 = lazy [0u .. 0xffffffffu]
    // let numbers_uint64 = lazy [0UL .. 0xffffffffffffffffUL]

    let generate n =
        [ for i in 0 .. n -> (pown 2 i) - 1 ]

    let toTestcase n = uint8 (n), sprintf "%uu8" n

    let n8 = generate 8 |> List.map toTestcase
    // let n16 = generate 16 |> List.map (toTestcase Uint16)
    // let n32 = generate 32 |> List.map (toTestcase Uint32)
    // let n64 = generate 64 |> List.map (toTestcase Uint64)

    for element in n8 do
        let n, str = element

        let actual =
            str
            |> Parse
            |> getProgramExpression

        let expected = Constant(Uint8 n)
        Assert.That(expected, Is.EqualTo(actual))

// assert v = (Uint8 n) |> ignore

[<Test>]
let test() =
    // Printf.eprintfn "%A" (Parse "1%2+3")
    // Printf.eprintfn "%A" (Parse "1/2+3")
    // Printf.eprintfn "%A" (Parse "1*2+3")
    // Printf.eprintfn "%A" (Parse "1%2-3")
    // Printf.eprintfn "%A" (Parse "1/2-3")
    // Printf.eprintfn "%A" (Parse "1*2-3")
    // Printf.eprintfn "%A" (Parse "1%2<<3")
    // Printf.eprintfn "%A" (Parse "1/2<<3")
    // Printf.eprintfn "%A" (Parse "1*2<<3")
    // Printf.eprintfn "%A" (Parse "1%2>>3")
    // Printf.eprintfn "%A" (Parse "1/2>>3")
    // Printf.eprintfn "%A" (Parse "1*2>>3")
    // Printf.eprintfn "%A" (Parse "1%2<3")
    // Printf.eprintfn "%A" (Parse "1/2==3")
    // Printf.eprintfn "%A" (Parse "1*2&3")
    // Printf.eprintfn "%A" (Parse "1*2^3")
    // Printf.eprintfn "%A" (Parse "1*2|3")
    // Printf.eprintfn "%A" (Parse "1&2^3")
    // Printf.eprintfn "%A" (Parse "")
    // Printf.eprintfn "%A" (Parse "1")
    // Printf.eprintfn "%A" (Parse "1++")
    // Printf.eprintfn "%A" (Parse "1--")
    // Printf.eprintfn "%A" (Parse "++1")
    // Printf.eprintfn "%A" (Parse "--1")
    // Printf.eprintfn "%A" (Parse "1*2")
    // Printf.eprintfn "%A" (Parse "1/2")
    // Printf.eprintfn "%A" (Parse "1%2")
    // Printf.eprintfn "%A" (Parse "1+2")
    // Printf.eprintfn "%A" (Parse "1-2")
    // Printf.eprintfn "%A" (Parse "1<2")
    // Printf.eprintfn "%A" (Parse "1<=2")
    // Printf.eprintfn "%A" (Parse "1>2")
    // Printf.eprintfn "%A" (Parse "1>=2")
    // Printf.eprintfn "%A" (Parse "1==2")
    // Printf.eprintfn "%A" (Parse "1!=2")
    // Printf.eprintfn "%A" (Parse "1&2")
    // Printf.eprintfn "%A" (Parse "1|2")
    // Printf.eprintfn "%A" (Parse "1^2")
    // Printf.eprintfn "%A" (Parse "(1)")
    // Printf.eprintfn "%A" (Parse "open SomeModule")
    // Printf.eprintfn "%A" (Parse "let a = 1")
    // Printf.eprintfn "%A" (Parse "~1*2")
    // Printf.eprintfn "%A" (Parse "tuple { 1; 2; }")
    // Printf.eprintfn "%A" (Parse "array { 1; 2; }")
    // Printf.eprintfn "%A" (Parse "dict { 1:1; 2:3; }")
    // Printf.eprintfn "%A" (Parse "module { 1 }")
    // Printf.eprintfn "%A" (Parse "1 // hola\n")
    // Printf.eprintfn "%A" (Parse "\"hola\"[1]")
    // Printf.eprintfn "%A" (Parse "hola[1]")
    // Printf.eprintfn "%A" (Parse "1 // hola")

    Printf.eprintfn "%A" (Parse "lambda () { 1 }")
    Printf.eprintfn "%A" (Parse "lambda (a0) { 1 }")
    Printf.eprintfn "%A" (Parse "lambda (a0, a1) { 1 }")
    Printf.eprintfn "%A" (Parse "lambda (a0, a1, a2) { 1+2 }")
    Printf.eprintfn "%A" (Parse "lambda (a0:int):string { 1 }")
    Printf.eprintfn "%A" (Parse "lambda (a0:string, a1:float) { 1 }")
    Printf.eprintfn "%A" (Parse "lambda (a0, a1, a2) { 1 }")


    // Printf.eprintfn "%A" (Parse "1")
    // Printf.eprintfn "%A" (Parse "0x2")
    // Printf.eprintfn "%A" (Parse "0b11")

// Printf.eprintfn "%A" (Parse "(hola:function<int>) -> 1")
// Printf.eprintfn "%A" (Parse "(hola:tuple<int>) -> 1")
// Printf.eprintfn "%A" (Parse "(hola:array<int>) -> 1")
// Printf.eprintfn "%A" (Parse "(hola:struct<int>) -> 1")
// Printf.eprintfn "%A" (Parse "hola (mundo:long) -> 2")
// Printf.eprintfn "%A" (Parse "(hola:int) mundo -> 3")
// Printf.eprintfn "%A" (Parse "(hola:int) (mundo:long) -> 4")
// Printf.eprintfn "%A" (Parse "(hola:int) -> 5")
// Printf.eprintfn "%A" (Parse "hola -> 6")
