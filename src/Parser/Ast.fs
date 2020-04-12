module AST

type Name = string

type Type =
    | NamedType of string
    | FunctionType of Type list
    | TupleType of Type list
    | ArrayType of Type
    | StructType of Type list
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

type Expression =
    | Unit
    | Constant of Literal
    | Variable of Name
    | PrefixExpression of string * Expression
    | PostfixExpression of string * Expression
    | InfixExpression of string * Expression * Expression
    | TupleLiteral of Expression list
    | ArrayLiteral of Expression list
    | DictionaryLiteral of (Expression * Expression) list
    | Lambda of Parameter list * Type * Expression
    | IndexedLookup of Expression * Expression
    | Module
    | Struct of StructField list
    | Union of UnionField list
    | While of Expression * Expression

    // | Let of Expression * Expression
    // | Match
    // | For

type Declaration =
    | Open of Name
    | Let of Name * Expression

type Program =
    Program of Declaration list * Expression

type SynExpr = unit

type Ident(text: string) =
    member x.idText = text
    override x.ToString() = text

type LongIdent = Ident list

type SynModuleOrNamespaceKind =
    | NamedModule
    | AnonModule

type range = unit

type SynAccess =
    | Public
    | Internal
    | Private

type SynBindingKind =

    /// A standalone expression in a module
    | StandaloneExpression

    /// A normal 'let' binding in a module
    | NormalBinding

    /// A 'do' binding in a module. Must have type 'unit'
    | DoBinding

type TypeName = Name

type SynAttribute =
    { TypeName: LongIdent

      ArgExpr: SynExpr

      /// Target specifier, e.g. "assembly", "module", etc.
      Target: Ident option

      /// Is this attribute being applied to a property getter or setter?
      AppliesToGetterAndSetter: bool

      Range: range }

type SynAttributeList =
    { Attributes: SynAttribute list
      Range: range }

type SynAttributes = SynAttributeList list

type SynBinding =
    Binding of accessibility: SynAccess option *
    kind: SynBindingKind *
    attrs: SynAttributes *
    expr: SynExpr

type SynTypeDefn = TypeDefn

type SynModuleDecl =
    | Let of isRecursive: bool * SynBinding list * range: range
    | Types of SynTypeDefn list * range: range
    | Open of longDotId: LongIdent * range: range

type SynModuleDecls = SynModuleDecl list

type SynModuleOrNamespace =
    SynModuleOrNamespace of longId: LongIdent *
    kind: SynModuleOrNamespaceKind *
    decls: SynModuleDecls *
    attribs: SynAttributes *
    accessibility: SynAccess option

type ParsedImplFileFragment =
    | AnonModule of SynModuleDecls
    | NamedModule of SynModuleOrNamespace

type ParsedImplFile = ParsedImplFile of ParsedImplFileFragment list
