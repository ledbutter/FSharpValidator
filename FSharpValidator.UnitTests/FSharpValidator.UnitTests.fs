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

[<TestCase("ひらがな・カタカナ、．漢字", true)>]
[<TestCase("あいうえお foobar", true)>]
[<TestCase("Foo＠example.com", true)>]
[<TestCase("1234abcDEｘｙｚ", true)>]
[<TestCase("ｶﾀｶﾅ", true)>]
[<TestCase("中文", true)>]
[<TestCase("æøå", true)>]
[<TestCase("abc", false)>]
[<TestCase("abc123", false)>]
[<TestCase("<>@\" *.", false)>]
let ``isMultiByteTest``(input : string, expected: bool) =
  let actual = isMultiByte input
  actual |> should equal expected

[<TestCase("!\"#$%&()<>/+=-_? ~^|.,@`{}[]", true)>]
[<TestCase("l-btn_02--active", true)>]
[<TestCase("abc123い", true)>]
[<TestCase("ｶﾀｶﾅﾞﾬ￩", true)>]
[<TestCase("あいうえお", false)>]
[<TestCase("００１１", false)>]
let ``isHalfWidthTest``(input: string, expected : bool) =
  let actual = isHalfWidth input
  actual |> should equal expected

[<TestCase("ひらがな・カタカナ、．漢字", true)>]
[<TestCase("３ー０　ａ＠ｃｏｍ", true)>]
[<TestCase("Ｆｶﾀｶﾅﾞﾬ", true)>]
[<TestCase("Good＝Parts", true)>]
[<TestCase("abc", false)>]
[<TestCase("abc123", false)>]
[<TestCase("!\"#$%&()<>/+=-_? ~^|.,@`{}[]", false)>]
let ``isFullWidthTest``(input : string, expected : bool) =
  let actual = isFullWidth input
  actual |> should equal expected

[<TestCase("ひらがなカタカナ漢字ABCDE", true)>]
[<TestCase("３ー０123", true)>]
[<TestCase("Ｆｶﾀｶﾅﾞﾬ", true)>]
[<TestCase("Good＝Parts", true)>]
[<TestCase("abc", false)>]
[<TestCase("abc123", false)>]
[<TestCase("!\"#$%&()<>/+=-_? ~^|.,@`{}[]", false)>]
[<TestCase("ひらがな・カタカナ、．漢字", false)>]
[<TestCase("１２３４５６", false)>]
[<TestCase("ｶﾀｶﾅﾞﾬ", false)>]
let ``isVariableWidthTest``(input : string, expected : bool) =
  let actual = isVariableWidth input
  actual |> should equal expected

[<TestCase("𠮷野𠮷", true)>]
[<TestCase("𩸽", true)>]
[<TestCase("ABC千𥧄1-2-3", true)>]
[<TestCase("吉野竈", false)>]
[<TestCase("鮪", false)>]
[<TestCase("ABC1-2-3", false)>]
let ``isSurrogatePairTest``(input : string, expected : bool) =
  let actual = isSurrogatePair input
  actual |> should equal expected

[<TestCase("Foo", [|"Foo"; "Bar"|], true)>]
[<TestCase("Bar", [|"Foo"; "Bar"|], true)>]
[<TestCase("Baz", [|"Foo"; "Bar"|], false)>]
let ``isInTest``(input : string, values : string[], expected : bool) =
  let actual = isIn input values
  actual |> should equal expected

[<TestCase("::1", IpVersion.Version4, false)>]
[<TestCase("127.0.0.1", IpVersion.Version4, true)>]
[<TestCase("0.0.0.0", IpVersion.Version4, true)>]
[<TestCase("255.255.255.255", IpVersion.Version4, true)>]
[<TestCase("abc", IpVersion.Version4, false)>]
[<TestCase("256.0.0.0", IpVersion.Version4, false)>]
[<TestCase("26.0.0.256", IpVersion.Version4, false)>]
[<TestCase("::1", IpVersion.Version6, true)>]
[<TestCase("2001:db8:0000:1:1:1:1:1", IpVersion.Version6, true)>]
[<TestCase("127.0.0.1", IpVersion.Version6, false)>]
[<TestCase("0.0.0.0", IpVersion.Version6, false)>]
[<TestCase("::1", IpVersion.Version6, true)>]
let ``isIpTest``(input : string, ipVersion : IpVersion, expected : bool) =
  let actual = isIp input ipVersion
  actual |> should equal expected
