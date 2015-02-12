module FSharpValidator.UnitTests

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open FSharpValidator.Functions

// Note on FsCheck tests: The NUnit test runner will still green-light failing tests with Check.Quick 
// even though it reports them as failing. Use Check.QuickThrowOnFailure instead.

open NUnitRunner

[<TestCase("123", true)>]
[<TestCase("Foo", false)>]
[<TestCase("123Foo123", false)>]
let ``IsNumericTest``(input : string, expected: bool) =
    let actual = isNumeric input
    actual |> should equal expected

[<TestCase("Foo", true)>]
[<TestCase("1Foo", false)>]
[<TestCase("123", false)>]
[<TestCase("1Foo\r\n12", false)>]
[<TestCase("Foo_Bar", false)>]
let ``IsAlphaTest``(input : string, expected: bool) =
    let actual = isAlpha input
    actual |> should equal expected

[<TestCase("foo", true)>]
[<TestCase("foo123", true)>]
[<TestCase("FOO", false)>]
[<TestCase("FOO123", false)>]
let ``isLowerCaseTest``(input : string, expected: bool) =
    let actual = isLowerCase input
    actual |> should equal expected

[<TestCase("foo", false)>]
[<TestCase("foo123", false)>]
[<TestCase("FOO", true)>]
[<TestCase("FOO123", true)>]
let ``isUpperCaseTest``(input : string, expected: bool) =
    let actual = isUpperCase input
    actual |> should equal expected

[<TestCase("123.123", true)>]
[<TestCase("123", true)>]
[<TestCase("", false)>]
let ``isFloatTest``(input : string, expected: bool) =
    let actual = isFloat input
    actual |> should equal expected

[<TestCase("10", 5, true)>]
[<TestCase("10", 2, true)>]
[<TestCase("5", 2, false)>]
[<TestCase("Foo", 2, false)>]
let ``isDivisibleTest``(input : string, by: int, expected: bool) =
    let actual = isDivisibleBy input by
    actual |> should equal expected

[<TestCase("ab", 1, 2, true)>]
[<TestCase("abc", 1, 2, false)>]
[<TestCase("", 1, 2, false)>]
let ``isLengthTest``(input : string, min: int, max: int, expected: bool) =
    let actual = isLength input min max
    actual |> should equal expected

[<TestCase("Foo", true)>]
[<TestCase("123", true)>]
[<TestCase("Foo@example.com", true)>]
[<TestCase("ｆｏｏ", false)>]
[<TestCase("１２３", false)>]
let ``isAsciiTest``(input : string, expected: bool) =
    let actual = isAscii input
    actual |> should equal expected
