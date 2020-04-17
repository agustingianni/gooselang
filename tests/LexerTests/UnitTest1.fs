module LexerTests

open NUnit.Framework
open FSharp.Text.Lexing


let tokenize input = LexBuffer<_>.FromString input |> Lexer.tokenize

[<Test>]
let TestLexer() =
    let elements = [
        "and", Parser.AND
        "break", Parser.BREAK
        "continue", Parser.CONTINUE
        "do", Parser.DO
        "else", Parser.ELSE
        "false", Parser.FALSE
        "for", Parser.FOR
        "if", Parser.IF
        "in", Parser.IN
        "let", Parser.LET
        "match", Parser.MATCH
        "module", Parser.MODULE
        "not", Parser.NOT
        "open", Parser.OPEN
        "or", Parser.OR
        "struct", Parser.STRUCT
        "then", Parser.THEN
        "to", Parser.TO
        "true", Parser.TRUE
        "union", Parser.UNION
        "unit", Parser.UNIT
        "while", Parser.WHILE

        "AnIdentifier", Parser.IDENT "AnIdentifier"
        "_AnIdentifier", Parser.IDENT "_AnIdentifier"

        // Operators
        "-"  , Parser.MINUS
        ","  , Parser.COMMA
        ";"  , Parser.SEMI
        "!"  , Parser.BANG
        "["  , Parser.LBRACK
        "]"  , Parser.RBRACK
        "("  , Parser.LPAREN
        ")"  , Parser.RPAREN
        "{"  , Parser.LCURLY
        "}"  , Parser.RCURLY
        "*"  , Parser.STAR
        "/"  , Parser.DIV
        "&"  , Parser.AMP
        "%"  , Parser.MOD
        "+"  , Parser.PLUS
        "<"  , Parser.LT
        "="  , Parser.ASSIGN
        "=="  , Parser.EQ
        ">"  , Parser.GT
        "~"  , Parser.WAVE
        "->" , Parser.ARROW
        ".." , Parser.DOT_DOT

        // Numbers.
        "1s8", Parser.INT8 1y
        "1u8", Parser.UINT8 1uy
        "1s16", Parser.INT16 1s
        "1u16", Parser.UINT16 1us
        "1s32", Parser.INT32 1
        "1u32", Parser.UINT32 1u
        "1s64", Parser.INT64 1L
        "1u64", Parser.UINT64 1UL

        "0x1s8", Parser.INT8 1y
        "0x1u8", Parser.UINT8 1uy
        "0x1s16", Parser.INT16 1s
        "0x1u16", Parser.UINT16 1us
        "0x1s32", Parser.INT32 1
        "0x1u32", Parser.UINT32 1u
        "0x1s64", Parser.INT64 1L
        "0x1u64", Parser.UINT64 1UL

        "0b1s8", Parser.INT8 1y
        "0b1u8", Parser.UINT8 1uy
        "0b1s16", Parser.INT16 1s
        "0b1u16", Parser.UINT16 1us
        "0b1s32", Parser.INT32 1
        "0b1u32", Parser.UINT32 1u
        "0b1s64", Parser.INT64 1L
        "0b1u64", Parser.UINT64 1UL

        "1.", Parser.IEEE64 1.
        "1.1", Parser.IEEE64 1.1
        "1.111", Parser.IEEE64 1.111
        "1.f", Parser.IEEE32 1.f
        "1.1f", Parser.IEEE32 1.1f
        "1.111f", Parser.IEEE32 1.111f

        "1.e2", Parser.IEEE64 1.e2
        "1.1e2", Parser.IEEE64 1.1e2
        "1.11e+2", Parser.IEEE64 1.11e+2
        "1.11e-2", Parser.IEEE64 1.11e-2


        "1", Parser.INT64 1L
        "true", Parser.TRUE
        "false", Parser.FALSE

        "\"hola\"", Parser.STRING "hola"
        "\"\\x41\"", Parser.STRING "\x41"
        "\"\\u4141\"", Parser.STRING "\u4141"
        "\"aaaaa\pa\"", Parser.STRING "aaaaa\pa"
        "// hola\n", Parser.EOF
    ]

    elements
    |> List.iter (fun (input, expected) ->
        let actual = tokenize input
        Assert.That(expected, Is.EqualTo(actual)))

[<EntryPoint>]
let main argv = 0