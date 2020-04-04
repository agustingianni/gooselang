module AST

// http://fssnip.net/lf

// Program
type Name = string

type TypeName = Name

type Comment = Comment of string

type Expression =
    | Boolean of bool
    | Integer of int64
    | Float of float
    | String of string
    | Unit
    | Identifier of Name
    | PrefixExpression of string * Expression
    | PostfixExpression of string * Expression
    | InfixExpression of string * Expression * Expression
    | TupleLiteral of Expression list
    | ArrayLiteral of Expression list
    | DictionaryLiteral of (Expression * Expression) list
    | Function of Expression list * Expression

    | Let of Expression * Expression
    | Module of string option
    // | While of Expression * Expression
    // Match
    // While
    // For
    // Struct
    // Union

type Declaration =
    | Open of Expression
    | Module of Expression
    | TopLet of Expression

// Program
//   Module*
//   Expression

type Program =
    | Program of Declaration list * Expression