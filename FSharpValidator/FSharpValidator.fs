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
            if currentValue = '-' && input.Length <= 1 then
                false
            elif currentValue <= '/' || currentValue >= ':' then
                false
            else
                if index + 1 = input.Length then
                    true
                else
                    isCharNumeric input (index + 1)
        isCharNumeric input 0

    let isInt (input : string) =
        isNumeric input

    let isFloat input = 
        let (res, floatVal) = System.Double.TryParse(input)
        res

    let isDivisibleBy input by =
        let (res, intVal) = System.Int32.TryParse(input)
        if (res = false) then
            false
        else
            intVal % by = 0

    let isLength (input : string) min max =
        let length = input.Length
        length >= min && length <= max
        
    let isAscii input =
        let rec isCharAscii (input : string) index =
            let currentValue = int32 (input.Chars(index))
            if currentValue > 127 then
                false
            else
                if index + 1 = input.Length then
                    true
                else
                    isCharAscii input (index + 1)
        isCharAscii input 0

    let isMultiByte input =
      Regex.IsMatch(input, "[^\x00-\x7F]")

    let isHalfWidth input =
      Regex.IsMatch(input, "[\u0020-\u007E\uFF61-\uFF9F\uFFA0-\uFFDC\uFFE8-\uFFEE0-9a-zA-Z]")

    let isFullWidth input =
      Regex.IsMatch(input, "[^\u0020-\u007E\uFF61-\uFF9F\uFFA0-\uFFDC\uFFE8-\uFFEE0-9a-zA-Z]")

    let isVariableWidth input =
      isHalfWidth <| input && isFullWidth <| input

    let isSurrogatePair input =
      Regex.IsMatch(input, "[\uD800-\uDBFF][\uDC00-\uDFFF]")



