module GooseLangParser

open FSharp.Text.Lexing

let parseTokens (input: string) =
    let buffer = LexBuffer<char>.FromString input
    seq {
        let mutable stop = false
        while not stop do
            let token = Lexer.tokenize buffer
            if token = Parser.EOF then stop <- true else yield token
    }
    |> List.ofSeq

let parseString (input: string) =
    let buffer = LexBuffer<char>.FromString(input)

    try
        Parser.program Lexer.tokenize buffer

    with e ->
        let pos = buffer.EndPos
        let line = pos.Line
        let column = pos.Column
        let lastToken = System.String(buffer.Lexeme)
        eprintfn "Parse failed at line %d, column %d" line column
        eprintfn "Input: %A" input
        exit 1

let parseFile (path: string) =
    let input = System.IO.File.ReadAllText(path)
    parseString input
