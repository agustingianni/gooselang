open System
open Colorful
open System.Drawing

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

    let program = line |> GooseLangParser.parseString
    Console.WriteLine(sprintf "Program: %A" program)

    let result = program |> Interpreter.execute
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

type ReplCommand =
    | Quit
    | Help
    | ToggleDebug
    | Clear
    | ConsoleReset
    | Invalid of string

type LineCommand = 
    | Command of ReplCommand
    | Script of string
    | Nothing

type Repl() =
    let mutable Stop = false
    let mutable Debug = false

    let parseReplCommand = function
        | "#quit" -> Quit
        | "#help" -> Help
        | "#debug" -> ToggleDebug
        | "#clear" -> Clear
        | "#reset" -> ConsoleReset
        | unknown -> Invalid(unknown)

    let parseInputLine (line:string) =
        if line.Length = 0 then
            Nothing
        else if line.StartsWith '#' then
            Command(parseReplCommand line)
        else
            Script line

    let printHelp() =
        Console.WriteLine("Commands:")
        Console.WriteLine("  #quit  -> Exits the repl.")
        Console.WriteLine("  #help  -> Shows this help.")
        Console.WriteLine("  #debug -> Toggle debug information.")
        Console.WriteLine("  #clear -> Clear VM state.")
        Console.WriteLine("  #reset -> Reset console.")

    member this.HandleQuitCommand () =
        Stop <- true

    member this.HandleHelpCommand () =
        printHelp()

    member this.HandleDebugCommand () =
        Debug <- not Debug
        let message = (if Debug then "Enabled" else "Disabled") + " debugging."
        Console.WriteLine(message)

    member this.HandleConsoleResetCommand () =
        Console.Clear()

    member this.HandleClearVMStateCommand () =
        ()

    member this.HandleInvalidCommand command =
        printfn "Ignoring unknown command %s" command

    member this.HandleCommand command =
        match command with
        | Quit -> this.HandleQuitCommand ()
        | Help -> this.HandleHelpCommand ()
        | ToggleDebug -> this.HandleDebugCommand ()
        | Clear -> this.HandleClearVMStateCommand ()
        | ConsoleReset -> this.HandleConsoleResetCommand ()
        | Invalid(command) -> this.HandleInvalidCommand command

    member this.HandleScript script =
        compileLine script

    member this.Run () =
        ReadLine.HistoryEnabled <- true

        while not Stop do
            let input = ReadLine.Read("[λ]: ")
            let commandType = parseInputLine input

            match commandType with
            | Command(command) -> this.HandleCommand command
            | Script(script) -> this.HandleScript script
            | Nothing -> ()

[<EntryPoint>]
let main argv =
    Console.WriteLine("Welcome to goose-lang repl!", Color.Blue)
    Console.WriteLine("Type #help to see a list of available commands.", Color.Blue)

    let repl = Repl()
    repl.Run()
    0
