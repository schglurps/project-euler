open System.Text.RegularExpressions
// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
// The use of "and" when writing out numbers is in compliance with British usage.

[<EntryPoint>]
let main argv =

    let firstNumbersMap =
        [ "one"; "two";"three"; "four";"five"; "six"; "seven"; "eight"; "nine"; "ten";
           "eleven";"twelve"; "thirteen";"fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen" ]
        |> List.mapi(fun i s -> (i + 1, s))
        |> Map.ofList

    let tensMap =
        [ "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" ]
        |> List.mapi (fun i s -> (i + 2, s))
        |> Map.ofList

    let rec toLetters number =
        match number with
        | _ when number < 20 -> firstNumbersMap.TryFind number |> Option.get
        | n when number < 100 ->
            let tens = n / 10
            let units = n % 10
            if units = 0 then
                tensMap.TryFind tens |> Option.get
            else
                sprintf "%s-%s" (tensMap.TryFind tens |> Option.get) (toLetters units)
        | n when number < 1000 ->
            let hundreds = n / 100
            let remainder = n % 100
            if remainder = 0 then
                sprintf "%s hundred" (firstNumbersMap.TryFind hundreds |> Option.get)
            else
                sprintf "%s hundred and %s" (firstNumbersMap.TryFind hundreds |> Option.get) (toLetters remainder)
        | _ -> "one thousand"

    let withoutSpaceAndHyphen s =
        Regex.Replace(s, "[-\\s]", "")

    { 1 .. 1000 }
    |> Seq.fold (fun acc cur ->
        let s = toLetters cur |> withoutSpaceAndHyphen
        acc + s.Length) 0
    |> printfn "%i"

    0
