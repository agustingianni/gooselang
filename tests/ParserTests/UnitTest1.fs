// https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-fsharp-with-nunit
module ParserTests

open NUnit.Framework

open FParsec
open Parser
open AST

// TODO: How can we test for things that should not parse?
// Example: parseIdentifier "0Hola" should not parse into Identifier.

let parserTest parser input =
    match run parser input with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error

[<TestFixture>]
type CommentTests() =

    [<Test>]
    member this.TestComment() =
        let expected = Comment "hola!"
        let actual = parserTest parseComment "//hola!"
        Assert.That(expected, Is.EqualTo(actual))

[<TestFixture>]
type ExpressionTests() =

    [<Test>]
    member this.TestUnit() =
        let elements = [
            "unit", Unit
            "true", Boolean true
            "false", Boolean false
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestZeros() =
        let elements = [
            "0", Integer 0L
            "0x0", Integer 0L
            "0.0", Float 0.0
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestIntegerLimits() =
        let elements = [
            "0x7fffffffffffffff", Integer 0x7fffffffffffffffL
            "9223372036854775807", Integer 9223372036854775807L
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestStrings() =
        let elements = [
            "\"\"", String ""
            "\"goose\"", String "goose"
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.IdentifierTests() =
        let elements = [
            "Example", Identifier "Example"
            "_Example", Identifier "_Example"
            "_0Example", Identifier "_0Example"
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestPostfixExpression() =
        let makeIntExpr op value =
            let _value = Integer (int64 value)
            PostfixExpression(op, _value)

        let elements = [
            "1++", makeIntExpr "inc" 1
            "1--", makeIntExpr "dec" 1
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestPostfixDecExpression() =
        let makeIntExpr op value =
            let _value = Integer (int64 value)
            PrefixExpression(op, _value)

        let makeBoolExpr op value =
            PrefixExpression(op, Boolean value)

        let elements = [
            "!false", makeBoolExpr "not" false
            "++1", makeIntExpr "inc" 1
            "--1", makeIntExpr "dec" 1
            "~1", makeIntExpr "bnot" 1
            "-1", makeIntExpr "negative" 1
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestInfixExpression() =
        let makeIntExpr op lhs rhs =
            let _lhs =  Integer (int64 lhs)
            let _rhs =  Integer (int64 rhs)
            InfixExpression(op, _lhs, _rhs)

        let elements = [
            "1 * 1", makeIntExpr "mul" 1 1
            "1 / 1", makeIntExpr "div" 1 1
            "1 % 1", makeIntExpr "mod" 1 1
            "1 + 1", makeIntExpr "plus" 1 1
            "1 - 1", makeIntExpr "minus" 1 1
            "1 << 1", makeIntExpr "lshift" 1 1
            "1 >> 1", makeIntExpr "rshift" 1 1
            "1 < 1", makeIntExpr "lt" 1 1
            "1 <= 1", makeIntExpr "le" 1 1
            "1 > 1", makeIntExpr "gt" 1 1
            "1 >= 1", makeIntExpr "ge" 1 1
            "1 == 1", makeIntExpr "eq" 1 1
            "1 != 1", makeIntExpr "ne" 1 1
            "1 & 1", makeIntExpr "band" 1 1
            "1 ^ 1", makeIntExpr "bxor" 1 1
            "1 | 1", makeIntExpr "bor" 1 1
            "1 and 1", makeIntExpr "and" 1 1
            "1 xor 1", makeIntExpr "xor" 1 1
            "1 or 1", makeIntExpr "or" 1 1
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestArrayExpressions() =
        let elements = [
            "[ 1 ]", ArrayLiteral [Integer 1L]
            "[ 1 ; 2 ]", ArrayLiteral [Integer 1L; Integer 2L]
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestTupleExpressions() =
        let elements = [
            "( 1 )", TupleLiteral [Integer 1L]
            "( 1 ; hello )", TupleLiteral [Integer 1L; Identifier "hello"]
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestDictionaryExpressions() =
        let make x y = (Integer x, Integer y)
        let elements = [
            "{ 1 : 2 }", DictionaryLiteral [ (make 1L 2L) ]
            "{ 1 : 2 ; 3 : 4}", DictionaryLiteral [ (make 1L 2L) ; (make 3L 4L) ]
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseExpression input
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestLet() =
        let elements = [
            "let name = value", Let (Identifier "name", Identifier "value")
            "let name = [1 ; 2]", Let (Identifier "name", ArrayLiteral [Integer 1L; Integer 2L])
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseLet input
            Printf.eprintf "%A" actual
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestFunction() =
        let elements = [
            "a -> 1", Function ([Identifier "a"], Integer 1L)
            "a -> 1 + 1", Function ([Identifier "a"], InfixExpression ("plus", Integer 1L, Integer 1L))
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseFunction input
            Printf.eprintf "%A" actual
            Assert.That(expected, Is.EqualTo(actual))
        )

    [<Test>]
    member this.TestDeclarations() =
        let elements = [
            "open System", Open (Identifier "System")
            "module { 1+1 }", Module (InfixExpression ("plus", Integer 1L, Integer 1L))
        ]

        elements |> List.iter (fun (input, expected) ->
            let actual = parserTest parseDeclaration input
            Printf.eprintf "%A" actual
            Assert.That(expected, Is.EqualTo(actual))
        )

    // [<Test>]
    // member this.TestProgram() =
    //     let code = """
    //     let a = 1
    //     let b = 2
    //     a+b
    //     """

    //     let elements = [
    //         "let cpepepepepep = 1", Unit
    //     ]

    //     elements |> List.iter (fun (input, expected) ->
    //         let actual = parserTest parseProgram input
    //         Printf.eprintf "%A" actual
    //         Assert.That(expected, Is.EqualTo(actual))
    //     )

// Program
//   module-elements
//     type-definition
//     module-definition
//     import-declaration
//     function-definition
//     value-definition

// [<Test>]
// let ParsePrograms =
//     let input = "a = []"
//     parseExpression input |> ignore
//     ()
