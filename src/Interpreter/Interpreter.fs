module Interpreter

open AST
open System.Collections.Generic

type Value = Type * Expression

type Environment = Dictionary<string, Expression>

module Operations =
    let onInt8 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Int8
        | "div" -> lhs / rhs |> Int8
        | "mod" -> lhs % rhs |> Int8
        | "add" -> lhs + rhs |> Int8
        | "sub" -> lhs - rhs |> Int8
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onInt16 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Int16
        | "div" -> lhs / rhs |> Int16
        | "mod" -> lhs % rhs |> Int16
        | "add" -> lhs + rhs |> Int16
        | "sub" -> lhs - rhs |> Int16
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onInt32 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Int32
        | "div" -> lhs / rhs |> Int32
        | "mod" -> lhs % rhs |> Int32
        | "add" -> lhs + rhs |> Int32
        | "sub" -> lhs - rhs |> Int32
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onInt64 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Int64
        | "div" -> lhs / rhs |> Int64
        | "mod" -> lhs % rhs |> Int64
        | "add" -> lhs + rhs |> Int64
        | "sub" -> lhs - rhs |> Int64
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onUint8 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Uint8
        | "div" -> lhs / rhs |> Uint8
        | "mod" -> lhs % rhs |> Uint8
        | "add" -> lhs + rhs |> Uint8
        | "sub" -> lhs - rhs |> Uint8
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onUint16 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Uint16
        | "div" -> lhs / rhs |> Uint16
        | "mod" -> lhs % rhs |> Uint16
        | "add" -> lhs + rhs |> Uint16
        | "sub" -> lhs - rhs |> Uint16
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onUint32 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Uint32
        | "div" -> lhs / rhs |> Uint32
        | "mod" -> lhs % rhs |> Uint32
        | "add" -> lhs + rhs |> Uint32
        | "sub" -> lhs - rhs |> Uint32
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onUint64 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Uint64
        | "div" -> lhs / rhs |> Uint64
        | "mod" -> lhs % rhs |> Uint64
        | "add" -> lhs + rhs |> Uint64
        | "sub" -> lhs - rhs |> Uint64
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onFloat op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Float
        | "div" -> lhs / rhs |> Float
        | "mod" -> lhs % rhs |> Float
        | "add" -> lhs + rhs |> Float
        | "sub" -> lhs - rhs |> Float
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

    let onFloat32 op lhs rhs =
        match op with
        | "mul" -> lhs * rhs |> Float32
        | "div" -> lhs / rhs |> Float32
        | "mod" -> lhs % rhs |> Float32
        | "add" -> lhs + rhs |> Float32
        | "sub" -> lhs - rhs |> Float32
        | "lt" -> lhs < rhs |> Boolean
        | "le" -> lhs <= rhs |> Boolean
        | "gt" -> lhs > rhs |> Boolean
        | "ge" -> lhs >= rhs |> Boolean
        | "ne" -> lhs <> rhs |> Boolean
        | "eq" -> lhs = rhs |> Boolean
        | _ -> failwith "Invalid operand."

type SimpleEvaluator() =
    let env = Stack<Environment>()

    let getLiteral =
        function
        | Constant(literal) -> literal
        | _ -> failwith "Expression is not a Constant."

    let getLiteralAsIndex =
        function
        | Int8(v) -> (int) v
        | Int16(v) -> (int) v
        | Int32(v) -> (int) v
        | Int64(v) -> (int) v
        | Uint8(v) -> (int) v
        | Uint16(v) -> (int) v
        | Uint32(v) -> (int) v
        | Uint64(v) -> (int) v
        | _ -> failwith "Invalid Literal."

    member this.EvalIndexedLookup expr index =
        let expr = this.EvalExpression expr
        let index = this.EvalExpression index

        match expr with
        | TupleLiteral(elements) ->
            let index =
                index
                |> getLiteral
                |> getLiteralAsIndex
            elements |> List.item index

        | ArrayLiteral(elements) ->
            let index =
                index
                |> getLiteral
                |> getLiteralAsIndex
            elements |> List.item index

        | DictionaryLiteral(elements) ->
            elements
            |> List.find (fun element -> fst element = index)
            |> snd

        | _ -> failwith "Non indexable expression."

    member this.EvalInfixExpression op lhs rhs =
        let lhs = this.EvalExpression lhs |> getLiteral
        let rhs = this.EvalExpression rhs |> getLiteral

        let result =
            match lhs, rhs with
            | Int8 l, Int8 r -> Operations.onInt8 op l r
            | Int16 l, Int16 r -> Operations.onInt16 op l r
            | Int32 l, Int32 r -> Operations.onInt32 op l r
            | Int64 l, Int64 r -> Operations.onInt64 op l r
            | Uint8 l, Uint8 r -> Operations.onUint8 op l r
            | Uint16 l, Uint16 r -> Operations.onUint16 op l r
            | Uint32 l, Uint32 r -> Operations.onUint32 op l r
            | Uint64 l, Uint64 r -> Operations.onUint64 op l r
            | Float l, Float r -> Operations.onFloat op l r
            | Float32 l, Float32 r -> Operations.onFloat32 op l r
            | _ -> failwith "Cannot evaluate infix operation."

        Constant(result)

    member this.EvalPrefixExpression op e0 =
        let value = this.EvalExpression e0 |> getLiteral

        let result =
            match op, value with
            | "not", Boolean(v) -> Boolean(not v)
            | "neg", Int8(v) -> Int8(-v)
            | "neg", Int16(v) -> Int16(-v)
            | "neg", Int32(v) -> Int32(-v)
            | "neg", Int64(v) -> Int64(-v)
            | "neg", Float(v) -> Float(-v)
            | "neg", Float32(v) -> Float32(-v)
            | _ -> failwith "Cannot evaluate prefix operation."

        Constant(result)

    member this.EvalPostfixExpression op expr =
        match op, expr with
        | _ -> failwith "Cannot evaluate postfix operation."

    member this.EvalVariableExpression name =
        let cur_env = env.Peek()
        let found, v0 = cur_env.TryGetValue name
        if not found then failwith "Variable not declared."
        v0

    member this.EvalConstant constant =
        let getConstantType =
            function
            | Boolean(_) -> NamedType "bool"
            | Int8(_) -> NamedType "i8"
            | Int16(_) -> NamedType "i16"
            | Int32(_) -> NamedType "i32"
            | Int64(_) -> NamedType "i64"
            | Uint8(_) -> NamedType "u8"
            | Uint16(_) -> NamedType "u16"
            | Uint32(_) -> NamedType "u32"
            | Uint64(_) -> NamedType "u64"
            | Float(_) -> NamedType "f64"
            | Float32(_) -> NamedType "f32"
            | String(_) -> NamedType "string"
            | Unit -> UnitType

        Value(getConstantType constant, Constant(constant))

    member this.EvalCallExpression name arguments =
        printfn "Calling function %s" name

        // Get the function by name from the current context.
        let cur_env = env.Peek()
        let found, f = cur_env.TryGetValue name
        if not found then failwith "Variable not declared."

        // Evaluate the body.
        match f with
        | Lambda(parameters, _, body) ->
            // Check that the parameter count matches.
            if List.length arguments <> List.length parameters then
                failwith "Number of parameters do not match."

            // Evaluate the arguments.
            let values = arguments |> List.map this.EvalExpression

            // Create a new context.
            let new_env = Environment()

            // Build a tuple with the parameter and its value.
            let zipped = List.zip parameters values

            for (parameter, value) in zipped do
                let name =
                    match parameter with
                        | UntypedParameter(name) -> name
                        | TypedParameter(name, _) -> name

                new_env.Add(name, value)

            // Push the new environment.
            env.Push new_env

            // Evaluate the body.
            let ret = this.EvalExpression body

            // Pop the used environment.
            env.Pop |> ignore

            ret

        | _ -> failwith "Not a function."

    member this.EvalExpression(expr: Expression): Expression =
        match expr with
        | Constant(_) ->
            expr

        | Variable(name) -> this.EvalVariableExpression name

        | PrefixExpression(op, e0) -> this.EvalPrefixExpression op e0

        | PostfixExpression(op, e0) ->
            let v0 = this.EvalExpression e0
            this.EvalPostfixExpression op v0

        | InfixExpression(op, e0, e1) -> this.EvalInfixExpression op e0 e1

        | TupleLiteral(elements) ->
            elements
            |> List.map this.EvalExpression
            |> TupleLiteral

        | ArrayLiteral(elements) ->
            elements
            |> List.map this.EvalExpression
            |> ArrayLiteral

        | DictionaryLiteral(elements) ->
            elements
            |> List.map (fun (key, value) -> (this.EvalExpression key, this.EvalExpression value))
            |> DictionaryLiteral

        | IndexedLookup(expr, index) -> this.EvalIndexedLookup expr index

        | CallExpression(name, args) ->
            this.EvalCallExpression name args

        | Lambda(_) ->
            expr

        | _ -> failwith "Shit"

    member this.EvalOpenDeclaration name = printfn "Opening module %s" name

    member this.EvalLetDeclaration name expr =
        let cur_env = env.Peek()
        if cur_env.ContainsKey name then failwith "Variable already declared."
        let val0 = this.EvalExpression expr
        printfn "Creating variable %s with value %A" name val0
        cur_env.Add(name, val0)

    member this.EvalDeclaration(decl: Declaration) =
        printfn "Evaluating declaration `%A`" decl
        match decl with
        | Open(name) -> this.EvalOpenDeclaration name
        | Let(name, expr) -> this.EvalLetDeclaration name expr

    member this.Eval(program: Program) (context: Environment) =
        env.Push context

        let programDeclarations = function
            | Program(decls, _) -> decls

        let programExpression = function
            | Program(_, expr) -> expr

        program
        |> programDeclarations
        |> List.iter this.EvalDeclaration

        let expr = program |> programExpression
        match expr with
        | Some expr -> this.EvalExpression expr
        | None -> Constant(Unit)

let execute (context: Environment) (program: Program) =
    let evaluator = SimpleEvaluator()
    evaluator.Eval program context
