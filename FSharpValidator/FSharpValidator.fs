namespace FSharpValidator

open System
open System.Text.RegularExpressions


module Functions =  

    let isAlpha input = Regex.IsMatch(input, "^[a-zA-Z]+$")

    let isLowerCase (input : string) = input.Equals(input.ToLower())

    let isUpperCase (input : string) = input.Equals(input.ToUpper())

    let isNumeric (input : string) =
        //not sure why this isn't just done with a Regex, but, whatever, good practice :)
        let rec isCharNumeric (input : string) index =
            let currentValue = input.Chars(index)
            match currentValue with
            | c when c = '-' && input.Length <= 1 -> false
            | c when c <= '/' || c >= ':' -> false
            | c when index + 1 = input.Length -> true
            | c -> isCharNumeric input (index + 1) 
        isCharNumeric input 0

    let isInt (input : string) =
        isNumeric input

    let isFloat input = 
        let (res, floatVal) = System.Double.TryParse(input)
        res

    let isDivisibleBy input by =
        let (res, intVal) = System.Int32.TryParse(input)
        match (res, intVal) with
        | (false, _) -> false
        | (true, x) -> x % by = 0


    let isLength (input : string) min max =
        let length = input.Length
        length >= min && length <= max

    let (|Ascii|_|) ch =
        match int32 ch with
        | x when x > 127 -> None
        | x -> Some x
        
    let isAscii input =
        let rec isCharAscii (input : string) index =
            match input.Chars(index) with
            | Ascii _ when index + 1 = input.Length -> true
            | Ascii _ -> isCharAscii input (index + 1)
            | _ -> false
        isCharAscii input 0