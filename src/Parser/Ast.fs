module AST

type Name = string

type Type =
    | NamedType of string
    | FunctionType of Type list
    | TupleType of Type list
    | ArrayType of Type
    | StructType of Type list
    | UnitType
    | UnknownType

type Parameter =
    | UntypedParameter of string
    | TypedParameter of string * Type

type StructField =
    | StructField of Name * Type

type UnionField =
    | TypedUnionField of Name * Type
    | UnTypedUnionField of Name

type Literal =
    | Boolean of bool
    | Int8 of int8
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | Uint8 of uint8
    | Uint16 of uint16
    | Uint32 of uint32
    | Uint64 of uint64
    | Float of float
    | Float32 of float32
    | String of string
    | Unit

type Expression =
    | Constant of Literal
    | Variable of Name
    | PrefixExpression of string * Expression
    | PostfixExpression of string * Expression
    | InfixExpression of string * Expression * Expression
    | CallExpression of Name * Expression list
    | IndexedLookup of Expression * Expression
    | DotLookupExpression of Expression * Name
    | TupleLiteral of Expression list
    | ArrayLiteral of Expression list
    | DictionaryLiteral of (Expression * Expression) list
    | Lambda of Parameter list * Type * Expression
    | Module
    | Struct of StructField list
    | Union of UnionField list
    | While of Expression * Expression

type Declaration =
    | Open of Name
    | Let of Name * Expression

type Program =
    Program of Declaration list * Expression option
