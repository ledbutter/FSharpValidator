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

// simple helper to cut down on repeated code
let testRunner cases func =
    cases |> List.map func

type TestCase = { Input : string; ExpectedResult : bool}

type TestCases = TestCase list

[<Test>]
let ``IsNumericTest``() =
    let testCases = ["123"; "Foo"; "123Foo123"]
    let expectedResults = [true; false; false]
    let results = testRunner testCases isNumeric
    results |> should equal expectedResults

[<Test>]
let ``IsAlphaTest``() =
    let testCases = ["Foo"; "1Foo"; "123"; "1Foo\r\n12"; "Foo_Bar"]
    let expectedResults = [true; false; false; false; false]
    let results = testRunner testCases isAlpha
    results |> should equal expectedResults

[<Test>]
let ``isLowerCaseTest``() =
    let testCases = [{Input="foo"; ExpectedResult=true};{Input="foo123"; ExpectedResult=true}; {Input="FOO"; ExpectedResult=false}; {Input="FOO123"; ExpectedResult=false}]
    let results = testRunner (testCases |> List.map (fun (T) -> T.Input)) isLowerCase
    results |> should equal (testCases |> List.map (fun (T) -> T.ExpectedResult))

[<Test>]
let ``isUpperCaseTest``() =
    let testCases = ["FOO"; "FOO123"; "foo"; "foo123"]
    let expectedResults = [true; true; false; false]
    let results = testRunner testCases isUpperCase
    results |> should equal expectedResults

//[<Test>]
//let ``FsCheck test 2 (string generator)``() =
//    let genString = 
//        gen {
//            let! a = Arb.generate<string>
//            return a
//        }
//   
//    let nUnitConfig = { Config.Default with Runner = nUnitRunner }
//
////    Check.Verbose ("Arbitrary strings", nUnitConfig, (Prop.forAll ( Arb.fromGen genString )
//    Check.One ("Arbitrary strings", nUnitConfig, (Prop.forAll ( Arb.fromGen genString )
//                (fun myString -> 
//                    myString = myString + " "
//                    |> Prop.trivial (myString.Length  = 0)
//                    |> Prop.classify (myString.Length = 1) "length = 1"
//                    |> Prop.classify (myString.Length = 2) "length = 2"
//                    |> Prop.classify (myString.Length = 3) "length = 3"
//                    |> Prop.classify (myString.Length = 4) "length = 4"
//                    |> Prop.classify (myString.Length = 5) "length = 5"
//                    |> Prop.classify (myString.Length = 6) "length = 6" 
//                    |> Prop.classify (myString.Length = 7) "length = 7" 
//                    |> Prop.classify (myString.Length > 7) "length = 8 or GT" )))
//
//// types for FsCheck test 3 (registering an arbitrary type for generation)
//type EvenInt = EvenInt of int with
//    static member op_Explicit(EvenInt i) = i
//
//type ArbitraryModifiers =
//    static member EvenInt() = 
//        Arb.from<int> 
//        |> Arb.filter (fun i -> i % 2 = 0) 
//        |> Arb.convert EvenInt int
//
//[<Test>]
//let ``FsCheck test 3 (registering an arbitrary type for generation)``() =
//    Arb.register<ArbitraryModifiers>() |> ignore
//
//    let ``generated even ints should be even`` (EvenInt i) = i % 2 = 1
//    Check.QuickThrowOnFailure ``generated even ints should be even``
//
//[<Test>]
//let ``FsCheck test 4 (and properties)``() =
//    Check.QuickThrowOnFailure ((1 = 1) |@ sprintf "1 = 1" .&. (2 = 3) |@ sprintf "2 != 3")
//
//[<Test>]
//let ``FsCheck test 5 (or properties)``() =
//    Check.QuickThrowOnFailure ((1 = 2) |@ sprintf "1 != 2" .|. (2 = 3) |@ sprintf "2 != 3")
//
//// type and spec for FsCheck test 6 (stateful testing)
//type Counter() =
//    let mutable n = 0
//    member x.Inc() = n <- n + 1
//    member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
//    member x.Get = n
//    member x.Reset() = n <- 0
//    override x.ToString() = n.ToString()
//
//open FsCheck.Commands
//
//let spec =
//    let inc = 
//        { new ICommand<Counter, int>() with
//            member x.RunActual actual = actual.Inc(); actual
//            member x.RunModel model = model + 1
//            member x.Post (actual, model) = model = actual.Get |@ sprintf "model = %i, actual = %i" model actual.Get
//            override x.ToString() = "inc"}
//    let dec = 
//        { new ICommand<Counter, int>() with
//            member x.RunActual actual = actual.Dec(); actual
//            member x.RunModel model = model - 1
//            member x.Post (actual, model) = model = actual.Get |@ sprintf "model = %i, actual = %i" model actual.Get
//            override x.ToString() = "dec"}
//    { new ISpecification<Counter, int> with
//        member x.Initial() = (new Counter(), 0)
//        member x.GenCommand _ = Gen.elements [inc;dec] }
//
//[<Test>]
//let ``FsCheck test 6a (stateful testing spec)``() =
//    Check.QuickThrowOnFailure (asProperty spec)
//
//// type and spec for FsCheck test 6b (stateful testing command series)
//
//let inc2 = 
//    { new ICommand<Counter, int>() with
//        member x.RunActual actual = actual.Inc(); actual
//        member x.RunModel model = model + 1
//        member x.Post (actual, model) = model = actual.Get |@ sprintf "model = %i, actual = %i" model actual.Get
//        override x.ToString() = "inc"}
//let dec2 = 
//    { new ICommand<Counter, int>() with
//        member x.RunActual actual = actual.Dec(); actual
//        member x.RunModel model = model - 1
//        member x.Post (actual, model) = model = actual.Get |@ sprintf "model = %i, actual = %i" model actual.Get
//        override x.ToString() = "dec"}
//
//let reset = 
//    { new ICommand<Counter, int>() with
//        member x.RunActual actual = actual.Reset(); actual
//        member x.RunModel model = 0
//        member x.Post (actual, model) = model = actual.Get |@ sprintf "model = %i, actual = %i" model actual.Get
//        override x.ToString() = "reset"}
//
//let spec2 genList =
//    { new ISpecification<Counter, int> with
//        member x.Initial() = (new Counter(), 0)
//        member x.GenCommand _ = Gen.elements genList }
//
//
//[<Test>]
//let ``FsCheck test 6b (stateful testing command series)``() =
//    let ``inc, dec, reset`` = [inc2; dec2; reset]
//    Check.QuickThrowOnFailure (asProperty (spec2 ``inc, dec, reset``))
//
//[<Test>]
//let ``FsUnit test 1``() =
//    [1; 2; 3] |> should equal [1; 2; 3; 4]
//
//[<Test>]
//let ``FsUnit test 2``() =
//    1 |> should not' (equal 1)
//
//[<Test>]
//let ``FsUnit test 3``() =
//    10.1 |> should (equalWithin 0.1) 10.22
//
//[<Test>]
//let ``FsUnit test 4 (should throw exception)``() =
//    (fun () -> 1 + 2 |> ignore) |> should throw typeof<System.Exception>
//
//[<Test>]
//let ``Unquote test 1``() =
//    test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>
