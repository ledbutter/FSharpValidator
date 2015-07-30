module FSharpValidator.UnitTests

// https://github.com/fsharp/FsUnit

open System
open FsUnit
open NUnit.Framework
open FSharpValidator.Functions
open System.Text.RegularExpressions

[<TestFixture>]
type ``FSharpValidatorTests`` () =

  [<TestCase("123", true)>]
  [<TestCase("Foo", false)>]
  [<TestCase("123Foo123", false)>]
  member x.``IsNumericTest``(input:string, expected: bool) =
      let actual = isNumeric input
      actual |> should equal expected

  [<TestCase("Foo", true)>]
  [<TestCase("1Foo", false)>]
  [<TestCase("123", false)>]
  [<TestCase("1Foo\r\n12", false)>]
  [<TestCase("Foo_Bar", false)>]
  member x.``IsAlphaTest``(input:string, expected: bool) =
      let actual = isAlpha input
      actual |> should equal expected

  [<TestCase("foo", true)>]
  [<TestCase("foo123", true)>]
  [<TestCase("FOO", false)>]
  [<TestCase("FOO123", false)>]
  member x.``isLowerCaseTest``(input:string, expected: bool) =
      let actual = isLowerCase input
      actual |> should equal expected

  [<TestCase("foo", false)>]
  [<TestCase("foo123", false)>]
  [<TestCase("FOO", true)>]
  [<TestCase("FOO123", true)>]
  member x.``isUpperCaseTest``(input:string, expected: bool) =
      let actual = isUpperCase input
      actual |> should equal expected

  [<TestCase("123.123", true)>]
  [<TestCase("123", true)>]
  [<TestCase("", false)>]
  member x.``isFloatTest``(input:string, expected: bool) =
      let actual = isFloat input
      actual |> should equal expected

  [<TestCase("10", 5, true)>]
  [<TestCase("10", 2, true)>]
  [<TestCase("5", 2, false)>]
  [<TestCase("Foo", 2, false)>]
  member x.``isDivisibleTest``(input:string, by: int, expected: bool) =
      let actual = isDivisibleBy input by
      actual |> should equal expected

  [<TestCase("ab", 1, 2, true)>]
  [<TestCase("abc", 1, 2, false)>]
  [<TestCase("", 1, 2, false)>]
  member x.``isLengthTest``(input:string, min: int, max: int, expected: bool) =
      let actual = isLength input min max
      actual |> should equal expected

  [<TestCase("Foo", true)>]
  [<TestCase("123", true)>]
  [<TestCase("Foo@example.com", true)>]
  [<TestCase("ｆｏｏ", false)>]
  [<TestCase("１２３", false)>]
  member x.``isAsciiTest``(input:string, expected: bool) =
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
  member x.``isMultiByteTest``(input:string, expected: bool) =
    let actual = isMultiByte input
    actual |> should equal expected

  [<TestCase("!\"#$%&()<>/+=-_? ~^|.,@`{}[]", true)>]
  [<TestCase("l-btn_02--active", true)>]
  [<TestCase("abc123い", true)>]
  [<TestCase("ｶﾀｶﾅﾞﾬ￩", true)>]
  [<TestCase("あいうえお", false)>]
  [<TestCase("００１１", false)>]
  member x.``isHalfWidthTest``(input: string, expected:bool) =
    let actual = isHalfWidth input
    actual |> should equal expected

  [<TestCase("ひらがな・カタカナ、．漢字", true)>]
  [<TestCase("３ー０　ａ＠ｃｏｍ", true)>]
  [<TestCase("Ｆｶﾀｶﾅﾞﾬ", true)>]
  [<TestCase("Good＝Parts", true)>]
  [<TestCase("abc", false)>]
  [<TestCase("abc123", false)>]
  [<TestCase("!\"#$%&()<>/+=-_? ~^|.,@`{}[]", false)>]
  member x.``isFullWidthTest``(input:string, expected:bool) =
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
  member x.``isVariableWidthTest``(input:string, expected:bool) =
    let actual = isVariableWidth input
    actual |> should equal expected

  [<TestCase("𠮷野𠮷", true)>]
  [<TestCase("𩸽", true)>]
  [<TestCase("ABC千𥧄1-2-3", true)>]
  [<TestCase("吉野竈", false)>]
  [<TestCase("鮪", false)>]
  [<TestCase("ABC1-2-3", false)>]
  member x.``isSurrogatePairTest``(input:string, expected:bool) =
    let actual = isSurrogatePair input
    actual |> should equal expected

  [<TestCase("Foo", [|"Foo"; "Bar"|], true)>]
  [<TestCase("Bar", [|"Foo"; "Bar"|], true)>]
  [<TestCase("Baz", [|"Foo"; "Bar"|], false)>]
  member x.``isInTest``(input:string, values:string[], expected:bool) =
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
  member x.``isIpTest``(input:string, ipVersion:IpVersion, expected:bool) =
    let actual = isIp input ipVersion
    actual |> should equal expected

  [<TestCase("foo@bar.com", true)>]
  [<TestCase("foo@bar.com.au", true)>]
  [<TestCase("foo+bar@bar.com", true)>]
  [<TestCase("invalidemail@", false)>]
  [<TestCase("invalid.com", false)>]
  [<TestCase("@invalid.com", false)>]
  member x.``isEmailTest``(input:string, expected:bool) =
    let actual = isEmail input
    actual |> should equal expected

  [<TestCase("deadBEEF", true)>]
  [<TestCase("ff0044", true)>]
  [<TestCase("abcdefg", false)>]
  [<TestCase("", false)>]
  [<TestCase("..", false)>]
  member x.``isHexadecimalTest``(input:string, expected:bool) =
    let actual = isHexadecimal input
    actual |> should equal expected

  [<TestCase("Foo1", true)>]
  [<TestCase("foo1", true)>]
  [<TestCase("Foo 1", false)>]
  [<TestCase("Foo_", false)>]
  member x.``isAlphanumericTest``(input:string, expected:bool) =
    let actual = isAlphanumeric input
    actual |> should equal expected

  [<TestCase("#ff0034", true)>]
  [<TestCase("#CCCCCC", true)>]
  [<TestCase("fff", true)>]
  [<TestCase("#fff", true)>]
  [<TestCase("#ff", false)>]
  [<TestCase("fff0", false)>]
  [<TestCase("#ff12FG", false)>]
  member x.``isHexColorTest`` (input:string, expected:bool) =
    let actual = isHexColor input
    actual |> should equal expected

  [<TestCase("Foo", true)>]
  [<TestCase("Bar", false)>]
  [<TestCase("Baz", false)>]
  member x.``equalsTest`` (input:string, expected:bool) =
    let actual = equals input "Foo"
    actual |> should equal expected

  [<TestCase(null, false)>]
  [<TestCase("", false)>]
  [<TestCase("Not a date", false)>]
  [<TestCase("01/01/2001", true)>]
  [<TestCase("50/20/2017", false)>]
  [<TestCase("01-01-2001", true)>]
  [<TestCase("2001/01/01", true)>]
  [<TestCase("01.01.2001", true)>]
  [<TestCase("Not05/01A/date/2001", false)>]
  member x.``isDateTest`` (input:string, expected:bool) =
    let actual = isDate input
    actual |> should equal expected

  static member isAfterData:Object[][] = 
    [|
      [| null; new DateTime(2011, 8, 4); false |]
      [| "2011-08-04"; new DateTime(2011, 8, 3); true|]
      [| "2011-08-10"; new DateTime(2011, 8, 3); true|]
      [| "2010-07-02"; new DateTime(2011, 8, 3); false|]
      [| "2011-08-03"; new DateTime(2011, 8, 3); false|]
      [| "foo"; new DateTime(2011, 8, 3); false|]
    |]

  [<TestCaseSource("isAfterData")>]
  member x.``isAfterTest`` (input:string, date:DateTime, expected:bool) =
    let actual = isAfter input date
    actual |> should equal expected

  static member isBeforeData:Object[][] =
    [|
      [|null; new DateTime(2011, 8, 4); false|]
      [|""; new DateTime(2011, 8, 4); false|]

      [|"2010-07-02"; new DateTime(2011, 8, 4); true|]
      [|"2010-08-04"; new DateTime(2011, 8, 4); true|]
      [|"2011-08-04"; new DateTime(2011, 8, 4); false|]
      [|"2011-09-10"; new DateTime(2011, 8, 4); false|]

      [|"2010-07-02"; new DateTime(2011, 7, 4); true|]
      [|"2010-08-04"; new DateTime(2011, 7, 4); true|]
      [|"2011-08-04"; new DateTime(2011, 7, 4); false|]
      [|"2011-09-10"; new DateTime(2011, 7, 4); false|]

      [|"foo"; new DateTime(2011, 7, 4); false|]
    |]

  [<TestCaseSource("isBeforeData")>]
  member x.``isBeforeTest`` (input:string, date:DateTime, expected:bool) =
    let actual = isBefore input date
    actual |> should equal expected

  [<TestCase(null, false)>]
  [<TestCase("Not a JSON string", false)>]
  [<TestCase("{\"username\":\"Admin\"}", true)>]
  [<TestCase("{username:\"Admin\"", false)>]
  member x.``isJsonTest`` (input:string, expected:bool) =
    let actual = isJson input
    actual |> should equal expected

  [<TestCase(null, true)>]
  [<TestCase("", false)>]
  [<TestCase("  ", false)>]
  [<TestCase("NULL", false)>]
  member x.``isNullTest`` (input:string, expected:bool) =
    let actual = isNull input
    actual |> should equal expected

  [<TestCase("Validator", "Valid", true)>]
  [<TestCase("Validator", "lid", true)>]
  [<TestCase("Validator", "", true)>]
  [<TestCase("", "", true)>]
  [<TestCase("", " ", false)>]
  member x.``containsTest`` (input:string, element:string, expected:bool) =
    let actual = contains input element
    actual |> should equal expected

  [<TestCase("Foo", "Foo", RegexOptions.None, true)>]
  [<TestCase("Bar", "B.*", RegexOptions.None, true)>]
  [<TestCase("Baz", "B.*", RegexOptions.None, true)>]
  [<TestCase("bar", "B.*", RegexOptions.None, false)>]
  [<TestCase("Foo", "B.*", RegexOptions.None, false)>]
  [<TestCase("foo", "Foo", RegexOptions.None, false)>]
  [<TestCase("Foo", "foo", RegexOptions.IgnoreCase, true)>]
  [<TestCase("\r\nFoo", "^Foo$", RegexOptions.Multiline, true)>]
  member x.``matchesTest`` (input:string, element:string, options:RegexOptions, expected:bool) =
    let actual = matches input element options
    actual |> should equal expected

  [<TestCase("507f1f77bcf86cd799439011", true)>]
  [<TestCase("507f1f77bcf86cd7994390", false)>]
  [<TestCase("507f1f77bcf86cd79943901z", false)>]
  [<TestCase("", false)>]
  [<TestCase("507f1f77bcf86cd799439011 ", false)>]
  [<TestCase("507s1f77bcf86cd799439011", false)>]
  member x.``isMongoIdTest`` (input:string, expected:bool) =
    let actual = isMongoId input
    actual |> should equal expected

  [<TestCase("Foo", 3, true)>]
  [<TestCase("Foo", 2, true)>]
  [<TestCase("Foo Bar", 3, true)>]
  [<TestCase("Foo", 5, false)>]
  [<TestCase("F", 2, false)>]
  [<TestCase("", 2, false)>]
  member x.``isByteLengthTest`` (input:string, min:int, expected:bool) =
    // no optional parameters for F#, so just hard-code in here the default value
    // which is always used in the C# test cases
    let actual = isByteLength input min System.Int32.MaxValue
    actual |> should equal expected

  [<TestCase("SGk=", true)>]
  [<TestCase("VmFsaWRhdG9y", true)>]
  [<TestCase("Foo", false)>]
  [<TestCase("Foo\r\nBar", false)>]
  [<TestCase("Foo?", false)>]
  member x.``isBase64Test`` (input:string, expected:bool) =
    let actual = isBase64 input
    actual |> should equal expected