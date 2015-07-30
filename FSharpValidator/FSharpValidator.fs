namespace FSharpValidator

open System
open System.Text.RegularExpressions
open System.ComponentModel.DataAnnotations
open System.Web.Script.Serialization

module Functions =  
    //private funcs
    let private rev xs = Seq.fold (fun acc x -> x::acc) [] xs


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

    let isMultiByte input =
      Regex.IsMatch(input, "[^\x00-\x7F]")

    let isHalfWidth input =
      Regex.IsMatch(input, "[\u0020-\u007E\uFF61-\uFF9F\uFFA0-\uFFDC\uFFE8-\uFFEE0-9a-zA-Z]")

    let isFullWidth input =
      Regex.IsMatch(input, "[^\u0020-\u007E\uFF61-\uFF9F\uFFA0-\uFFDC\uFFE8-\uFFEE0-9a-zA-Z]")

    let isVariableWidth input =
      isHalfWidth <| input && isFullWidth <| input

    let isSurrogatePair input =
      // need to escape the value, due to bug that will be fixed in v4.0: https://github.com/Microsoft/visualfsharp/issues/338
      Regex.IsMatch(input, @"[\uD800-\uDBFF][\uDC00-\uDFFF]")

    let isIn input values = 
      values
      |> Array.exists (fun s -> s = input)

    type IpVersion =
      | Version4=0
      | Version6=1

    let isIp input ipVersion =
      match ipVersion with
      | IpVersion.Version4 ->
        let isMatch = Regex.IsMatch(input, "^(\d?\d?\d)\.(\d?\d?\d)\.(\d?\d?\d)\.(\d?\d?\d)$")
        match isMatch with
        | true ->
          let parts = input.Split('.')
          parts
          |> Array.map (fun p -> int p)
          |> Array.max <= 255
        | _ -> false
      | IpVersion.Version6 ->
        Regex.IsMatch(input, "^::|^::1|^([a-fA-F0-9]{1,4}::?){1,7}([a-fA-F0-9]{1,4})$")
      | _ -> failwith "Unexpected IpVersionType"

    let isEmail input =
      let emailAttribute = new EmailAddressAttribute()
      emailAttribute.IsValid(input)

    let isHexadecimal input =
      Regex.IsMatch(input, "^[0-9a-fA-F]+$")

    let isAlphanumeric input =
      Regex.IsMatch(input, "^[a-zA-Z0-9]+$")

    let isHexColor input =
      Regex.IsMatch(input, "^#?(?:[0-9a-fA-F]{3}){1,2}$")

    let equals (input:string) (comparison:string) =
      input.Equals(comparison)

    let isDate input =
      let result, date = DateTime.TryParse(input)
      result

    let isAfter input date =
      match DateTime.TryParse(input) with
      | (false, _) -> false
      | (true, parsedDate) -> date < parsedDate

    let isBefore input date =
      match DateTime.TryParse(input) with
      | (false, _) -> false
      | (true, parsedDate) -> date > parsedDate

    let isJson input =
      let serializer = new JavaScriptSerializer()
      try
        serializer.Deserialize<Object>(input) |> ignore
        true
      with 
        | :? System.ArgumentException -> false

    let isNull input =
      match input with
      | null -> true
      | _ -> false

    let contains (input:string) (element:string) =
      input.Contains(element)

    let matches (input:string) (pattern:string) (options:RegexOptions) =
      // C# implementation has options with a default value of None
      // but F# doesn't allow optional parameters except on member methods
      Regex.IsMatch(input, pattern, options)

    let isMongoId (input:string) =
      input.Length = 24 && (isHexadecimal input)

    let isByteLength (input:string) min max =
      // C# implementation has max with a default value of int.MaxValue
      // but, again, F# doesn't allow optional parameters except on member methods
      input.Length >= min && input.Length <= max

    let isBase64 (input:string) =
      //thanks, StackOverflow! http://stackoverflow.com/q/6309379/1346943
      match input.Length % 4 with
      | 0 -> 
        Regex.IsMatch(input, @"^[a-zA-Z0-9\+/]*={0,3}$")
      | _ -> false

    let isCreditCard (input:string) =
      let cleansedInput = input.Replace(" ", "").Replace("-", "")

      let validCreditCardChar c =
        c >= '0' && c <= '9'

      if isNumeric cleansedInput then
        let sumOfDigits = 
          cleansedInput |> Seq.where validCreditCardChar 
          |> rev
          |> Seq.mapi (fun i e -> ((int)e - 48) * if i % 2 = 0 then 1 else 2)
          |> Seq.sumBy (fun e -> e/10 + e%10)
        sumOfDigits % 10 = 0
      else
        false