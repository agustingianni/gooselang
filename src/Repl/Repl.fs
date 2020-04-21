open System
open Colorful
open System.Drawing
open System.Collections.Generic

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
    | ShowEnvironment
    | Invalid of string

type LineCommand =
    | Command of ReplCommand
    | Script of string
    | Nothing

type Repl() =
    let mutable Stop = false
    let mutable Debug = false

    // Keep track of any bindings declared inside our scripts.
    let context = Interpreter.Environment()

    let parseReplCommand = function
        | "#quit" -> Quit
        | "#help" -> Help
        | "#debug" -> ToggleDebug
        | "#clear" -> Clear
        | "#reset" -> ConsoleReset
        | "#env" -> ShowEnvironment
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
        Console.WriteLine("  #env   -> Show bindings.")

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
        context.Clear()

    member this.HandleInvalidCommand command =
        printfn "Ignoring unknown command %s" command

    member this.HandleCommand command =
        match command with
        | Quit -> this.HandleQuitCommand ()
        | Help -> this.HandleHelpCommand ()
        | ToggleDebug -> this.HandleDebugCommand ()
        | Clear -> this.HandleClearVMStateCommand ()
        | ConsoleReset -> this.HandleConsoleResetCommand ()
        | ShowEnvironment -> this.HandleShowEnvironmentCommand ()
        | Invalid(command) -> this.HandleInvalidCommand command

    member this.HandleShowEnvironmentCommand () =
        for binding in context do
            printfn "%A -> %A" binding.Key binding.Value

    member this.HandleScript script =
        let program = script |> GooseLangParser.parseString
        Console.WriteLine(sprintf "Program: %A" program)

        let result = Interpreter.execute context program
        Console.WriteLine(sprintf "Result: %A" result)

    member this.Run () =
        ReadLine.HistoryEnabled <- true

        while not Stop do
            let input = ReadLine.Read("[λ]: ")
            let command = parseInputLine input

            match command with
            | Command(name) -> this.HandleCommand name
            | Script(script) -> this.HandleScript script
            | Nothing -> ()

[<EntryPoint>]
let main argv =
    Console.WriteLine("Welcome to goose-lang repl!", Color.Blue)
    Console.WriteLine("Type #help to see a list of available commands.", Color.Blue)

    let repl = Repl()
    repl.Run()
    0
