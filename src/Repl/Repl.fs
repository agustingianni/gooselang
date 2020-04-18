open System
open Colorful
open System.Drawing

let printHelp() =
    Console.WriteLine("Commands:")
    Console.WriteLine("  #quit  -> Exits the repl.")
    Console.WriteLine("  #help  -> Shows this help.")
    Console.WriteLine("  #debug -> Toggle debug information.")

type Repl() =
    member this.Stop = false
    member this.Debug = false

type CommandHistory() =
    let elements = ResizeArray<string>()
    let mutable current = 0

    member this.AddElement element =
        elements.Add element
        current <- elements.Count

    member this.NextElement() =
        if elements.Count > 0 && current + 1 < elements.Count then
            current <- current + 1
            elements.[current]
        else
            ""

    member this.PreviousElement() =
        if elements.Count > 1 && current > 0 then
            current <- current - 1
            let element = elements.[current]
            element
        else
            ""

let compileLine line =
    Console.WriteLine(sprintf "Read line: %s" line)

    let program = GooseLangParser.parseString line

    Console.WriteLine(sprintf "Program: %A" program)

    let result = Interpreter.execute program
    
    Console.WriteLine(sprintf "Result: %A" result)

let readLine (commands: CommandHistory) =
    let chars = ResizeArray<char>()
    let mutable stop = false

    while not stop do
        let key = Console.ReadKey()
        if key.Key = ConsoleKey.Enter then
            stop <- true
        else if

            key.Key = ConsoleKey.UpArrow then
            chars.Clear()

            let current = commands.PreviousElement()
            current |> chars.AddRange

            Console.SetCursorPosition(0, Console.WindowHeight - 1)
            let line =
                sprintf "\r[λ]: %s" current

            let whites = String(' ', Console.WindowWidth - line.Length - 5)
            printf "%s%s" line whites
        else if

            key.Key = ConsoleKey.DownArrow then
            chars.Clear()
            let current = commands.NextElement()
            current |> chars.AddRange

            Console.SetCursorPosition(0, Console.WindowHeight - 1)
            let line =
                sprintf "\r[λ]: %s" current

            let whites = String(' ', Console.WindowWidth - line.Length - 5)
            printf "%s%s" line whites
        else if

            key.Key <> ConsoleKey.Backspace then
            chars.Add key.KeyChar

    chars.ToArray() |> String


[<EntryPoint>]
let main argv =
    Console.WriteLine("Welcome to goose-lang repl!", Color.DarkRed)
    Console.WriteLine("Type #help to see a list of available commands.", Color.DarkRed)

    let state = Repl()

    let mutable stop = false
    let mutable debug = false

    ReadLine.HistoryEnabled <- true

    while not stop do
        let input = ReadLine.Read("[λ]: ")

        match input with
        | "#quit" ->
            stop <- true

        | "#help" ->
            printHelp()

        | "#debug" ->
            debug <- not debug
            let message =
                (if debug then "Enabled"
                 else "Disabled")
                + " debugging."
            Console.WriteLine(message)

        | "" ->
            Console.WriteLine("")

        | _ ->
            compileLine input

    0
