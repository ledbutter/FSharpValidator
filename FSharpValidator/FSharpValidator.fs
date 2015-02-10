namespace FSharpValidator

open System.Text.RegularExpressions


module Functions =

    let isAlpha input = Regex.IsMatch(input, "^[a-zA-Z]+$")

    let isLowerCase (input : string) = input.Equals(input.ToLower())

    let isUpperCase (input : string) = input.Equals(input.ToUpper())

    let isNumeric (input : string) =
        //not sure why this isn't just done with a Regex, but, whatever, good practice :)
        let rec isCharNumeric (input : string) index =
            let currentValue = input.Substring(index, index + 1)
            if currentValue = "-" && input.Length <= 1 then
                false
            elif currentValue <= "/" || currentValue >= ":" then
                false
            else
                if index + 2 = input.Length then
                    true
                else
                    isCharNumeric input (index + 1)
        isCharNumeric input 0

    let isInt (input : string) =
        isNumeric input