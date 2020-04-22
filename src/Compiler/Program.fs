open Parser
open System

[<EntryPoint>]
let main argv =
    let script = "
    let double = lambda(a) {
        let res = a + a
        res
    }
    "
    let program = script |> GooseLangParser.parseString
    Console.WriteLine(sprintf "Program: %A" program)

    let context = Interpreter.Environment()
    let result = Interpreter.execute context program
    Console.WriteLine(sprintf "Result: %A" result)

    0