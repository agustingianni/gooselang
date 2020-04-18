module GooseLangParser

open FSharp.Text.Lexing

let parseString (input: string) =
    let tokenized = LexBuffer<char>.FromString(input)

    try
        Parser.program Lexer.tokenize tokenized

    with e ->
        let pos = tokenized.EndPos
        let line = pos.Line
        let column = pos.Column
        let lastToken = System.String(tokenized.Lexeme)
        eprintfn "Parse failed at line %d, column %d" line column
        eprintfn "Input: %A" input
        exit 1

let parseFile (path: string) =
    let input = System.IO.File.ReadAllText(path)
    parseString input
