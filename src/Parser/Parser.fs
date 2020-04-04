module Parser

open System.Collections.Generic
open FParsec
open AST

// Parsing utilities.
let elementsBetweenStrings sOpen sClose pElement elementSeparator =
    let pElementWS = spaces >>. pElement .>> spaces
    let separatedElements = sepBy pElementWS (pstring elementSeparator)
    between (pstring sOpen) (pstring sClose) separatedElements

let keywords = [
    "and" ; "break" ; "case" ; "continue" ; "default" ; "do" ; "else"
    "false" ; "for" ; "if" ; "in" ; "not" ; "or" ; "return" ; "struct"
    "switch" ; "true" ; "union" ; "while" ; "xor" ; "let" ; "match" ; "type"
]

let keywordsSet = new HashSet<string>(keywords)
let isKeyword value = keywordsSet.Contains(value)

let ws = spaces

let skipStringWS s = skipString s .>> ws

// Forward declarations.
let parseExpression, pExprImpl = createParserForwardedToRef<Expression, unit>()

// Comments.
let parseComment: Parser<Comment, unit> =
    skipString "//" >>. restOfLine true |>> Comment

// Literals.
let parseUnit =
    pstring "unit" >>% Unit

let parseBoolean =
    let parseTrue = stringReturn "true" true
    let parseFalse = stringReturn "false" false
    (parseTrue <|> parseFalse) |>> Boolean

let parseNumber =
    let format = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.DefaultInteger
    numberLiteral format "integer" |>> fun p ->
        match p.IsInteger with
        | true -> Integer(int64 p.String)
        | false -> Float(float p.String)

let parseString =
    let escapeToString =
        function
        | 'n' -> "\n"
        | 'r' -> "\r"
        | 't' -> "\t"
        | c -> string c

    let normalCharacter = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedCharacter = pstring "\\" >>. (anyOf "\\\"nrt" |>> escapeToString)
    let quote = (pstring "\"")
    let stringContent = (stringsSepBy normalCharacter escapedCharacter)
    between quote quote stringContent |>> String

let parseArray =
    let arrayOpen = "["
    let arrayClose = "]"
    elementsBetweenStrings arrayOpen arrayClose parseExpression ";" |>> ArrayLiteral

let parseTuple =
    let tupleOpen = "("
    let tupleClose = ")"
    elementsBetweenStrings tupleOpen tupleClose parseExpression ";" |>> TupleLiteral

let parseDictionary =
    let parseEntry =
        parseExpression .>> skipString ":" .>> ws .>>. parseExpression

    let dictOpen = "{"
    let dictClose = "}"
    elementsBetweenStrings dictOpen dictClose parseEntry ";" |>> DictionaryLiteral

// Identifiers.
let parseIdentifier: Parser<Expression, unit> =
    let rawIdentifier =
        let isAsciiIdStart = fun c -> isAsciiLetter c || c = '_'
        let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
        let options =
            IdentifierOptions
                (isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue,
                 normalization = System.Text.NormalizationForm.FormKC, normalizeBeforeValidation = true,
                 allowAllNonAsciiCharsInPreCheck = true)

        identifier options

    let toIdentifier s =
        if isKeyword s then fail "Identifier is a keyword" else preturn s |>> Identifier

    rawIdentifier >>= toIdentifier

let parseLet = 
    // let <id> = <body>
    skipStringWS "let" >>. parseIdentifier .>> ws .>> skipStringWS "=" .>>. parseExpression |>> Let

// Declarations.
let parseOpen =
    skipStringWS "open" .>> ws >>. parseIdentifier |>> Open

// Definitions.
let parseModule = 
    let parseBody =
        skipStringWS "{" >>. parseExpression .>> skipStringWS "}"

    // module { ... }
    skipStringWS "module" .>> ws >>. parseBody |>> Module

let parseTopLet =
    parseLet |>> TopLet

let parseDeclaration = choice [
    parseOpen
    parseModule
    parseTopLet
]

// Expressions.
let parseFunction =
    many parseIdentifier .>> ws .>> skipStringWS "->" .>>. parseExpression |>> Function

let buildExpressionParser impl =
    let atomExpr = choice [
        parseBoolean
        parseNumber
        parseString
        parseUnit
        parseIdentifier
        parseArray
        parseTuple
        parseDictionary
    ]

    // A higher precedence number means it binds more tightly
    //
    // Associativity determines how operators of the same precedence
    // are grouped in the absence of parentheses.
    //
    // Operators may be associative (meaning the operations can be grouped arbitrarily),
    // left-associative (meaning the operations are grouped from the left),
    // right-associative (meaning the operations are grouped from the right) or
    // non-associative (meaning operations cannot be chained, often because the output
    // type is incompatible with the input types).
    //
    // https://docs.microsoft.com/en-us/cpp/cpp/cpp-built-in-operators-precedence-and-associativity?view=vs-2019
    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    let prefixExpr name = fun x -> PrefixExpression(name, x)
    let postfixExpr name = fun x -> PostfixExpression(name, x)
    let infixExpr name = fun x y -> InfixExpression(name, x, y)

    // Associative postfix operators are left to right associativity.
    opp.AddOperator(PostfixOperator("++", ws, 13, true, postfixExpr "inc"))
    opp.AddOperator(PostfixOperator("--", ws, 13, true, postfixExpr "dec"))

    // Associative prefix operators are right to left associativity.
    opp.AddOperator(PrefixOperator("++", ws, 12, true, prefixExpr "inc"))
    opp.AddOperator(PrefixOperator("--", ws, 12, true, prefixExpr "dec"))
    opp.AddOperator(PrefixOperator("~", ws, 12, true, prefixExpr "bnot"))
    opp.AddOperator(PrefixOperator("!", ws, 12, true, prefixExpr "not"))
    opp.AddOperator(PrefixOperator("-", ws, 12, true, prefixExpr "negative"))

    // Left to right associativity.
    opp.AddOperator(InfixOperator("*", ws, 11, Associativity.Left, infixExpr "mul"))
    opp.AddOperator(InfixOperator("/", ws, 11, Associativity.Left, infixExpr "div"))
    opp.AddOperator(InfixOperator("%", ws, 11, Associativity.Left, infixExpr "mod"))
    opp.AddOperator(InfixOperator("+", ws, 10, Associativity.Left, infixExpr "plus"))
    opp.AddOperator(InfixOperator("-", ws, 10, Associativity.Left, infixExpr "minus"))

    opp.AddOperator(InfixOperator("<<", ws, 9, Associativity.Left, infixExpr "lshift"))
    opp.AddOperator(InfixOperator(">>", ws, 9, Associativity.Left, infixExpr "rshift"))

    opp.AddOperator(InfixOperator("<", ws, 8, Associativity.Left, infixExpr "lt"))
    opp.AddOperator(InfixOperator("<=", ws, 8, Associativity.Left, infixExpr "le"))
    opp.AddOperator(InfixOperator(">", ws, 8, Associativity.Left, infixExpr "gt"))
    opp.AddOperator(InfixOperator(">=", ws, 8, Associativity.Left, infixExpr "ge"))
    opp.AddOperator(InfixOperator("==", ws, 7, Associativity.Left, infixExpr "eq"))
    opp.AddOperator(InfixOperator("!=", ws, 7, Associativity.Left, infixExpr "ne"))

    opp.AddOperator(InfixOperator("&", ws, 6, Associativity.Left, infixExpr "band"))
    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, infixExpr "bxor"))
    opp.AddOperator(InfixOperator("|", ws, 4, Associativity.Left, infixExpr "bor"))

    opp.AddOperator(InfixOperator("and", ws, 3, Associativity.Left, infixExpr "and"))
    opp.AddOperator(InfixOperator("xor", ws, 2, Associativity.Left, infixExpr "xor"))
    opp.AddOperator(InfixOperator("or", ws, 1, Associativity.Left, infixExpr "or"))

    // This is the thing to parse between the operators.
    opp.TermParser <- atomExpr .>> ws
    impl := opp.ExpressionParser

buildExpressionParser pExprImpl

// Main entry point for the parser.

// - open declaration
// - let definitions
// - type definitions
// - module definitions
let parseProgram =
    pipe2 (many parseDeclaration) parseExpression (
        fun declarations expression -> Program(declarations, expression)
    )
    
