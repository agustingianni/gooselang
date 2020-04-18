module Interpreter

open AST

let evalLiteral (lit: Literal) =
    match lit with
    | Int8(v) -> int v
    | Int16(v) -> int v
    | Int32(v) -> int v
    | Int64(v) -> int v
    | _ -> failwith "Cannot evaluate literal."

let evalInfixExpression op lhs rhs =
    match op with
    | "add" -> lhs + rhs
    | "div" -> lhs / rhs
    | "mod" -> lhs % rhs
    | "mul" -> lhs * rhs
    | "sub" -> lhs - rhs
    | _ -> failwith "Cannot evaluate infix operation."

let rec evalExpression (expr: Expression) =
    match expr with
    | Constant(lit) -> evalLiteral lit
    | InfixExpression(op, e0, e1) ->
        let lhs = evalExpression e0
        let rhs = evalExpression e1
        evalInfixExpression op lhs rhs
    | _ -> 0

let execute (program: Program) =
    match program with
    | Program(_, expression) -> evalExpression expression
