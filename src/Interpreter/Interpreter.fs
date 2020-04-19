module Interpreter

open AST
open System.Collections.Generic

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
    let env = new Dictionary<string, Literal>()

    member this.EvalInfixExpression op lhs rhs =
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

    member this.EvalPrefixExpression op expr =
        match op, expr with
        | "not", Boolean(v) -> Boolean(not v)
        | "neg", Int8(v) -> Int8(-v)
        | "neg", Int16(v) -> Int16(-v)
        | "neg", Int32(v) -> Int32(-v)
        | "neg", Int64(v) -> Int64(-v)
        | "neg", Float(v) -> Float(-v)
        | "neg", Float32(v) -> Float32(-v)
        | _ -> failwith "Cannot evaluate prefix operation."

    member this.EvalPostfixExpression op expr =
        match op, expr with
        | _ -> failwith "Cannot evaluate postfix operation."

    member this.EvalVariableExpression name =
        let found, v0 = env.TryGetValue name
        if not found then failwith "Variable not declared."
        v0

    member this.EvalExpression(expr: Expression) =
        printfn "EXPR: %A" expr

        match expr with
        | Constant(cons) -> cons

        | PrefixExpression(op, e0) ->
            let v0 = this.EvalExpression e0
            this.EvalPrefixExpression op v0

        | PostfixExpression(op, e0) ->
            let v0 = this.EvalExpression e0
            this.EvalPostfixExpression op v0

        | InfixExpression(op, e0, e1) ->
            let v0 = this.EvalExpression e0
            let v1 = this.EvalExpression e1
            this.EvalInfixExpression op v0 v1

        | Variable(name) -> this.EvalVariableExpression name

        | _ -> failwith "Shit"

    member this.EvalOpenDeclaration name = printfn "Opening module %s" name

    member this.EvalLetDeclaration name expr =
        if env.ContainsKey name then failwith "Vairiable already declared."
        let val0 = this.EvalExpression expr
        printfn "Creating variable %s with value %A" name val0
        env.Add(name, val0)

    member this.EvalDeclaration(decl: Declaration) =
        printfn "Evaluating declaration `%A`" decl
        match decl with
        | Open(name) -> this.EvalOpenDeclaration name
        | Let(name, expr) -> this.EvalLetDeclaration name expr

    member this.Eval(program: Program) =
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
        | None -> Unit

let execute (program: Program) =
    let evaluator = SimpleEvaluator()
    evaluator.Eval program
