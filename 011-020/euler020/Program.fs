// n! means n × (n − 1) × ... × 3 × 2 × 1

// For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
// and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

// Find the sum of the digits in the number 100!

[<EntryPoint>]
let main argv =

    let getDigits n =
        let rec inner n digitList =
            let digit = n % 10
            let quotient = n / 10
            let newDigitList = digit::digitList
            if quotient = 0 then
                newDigitList
            else
                inner quotient newDigitList

        inner n []

    let multiplyBy (n: int) (digits: int list) =
        let rec innerMul (acc: int list) (digits: int list) remainder =
            match digits with
            | x::xs -> 
                let r = x * n + remainder
                let newDigit = r % 10
                let newRemainder = r / 10
                innerMul (newDigit::acc) xs newRemainder
            | [] ->
                if remainder = 0 then
                    acc
                else
                    (getDigits remainder) @ acc

        innerMul [] (digits |> List.rev) 0

    { 11 .. 100 }
    |> Seq.fold (fun acc cur -> multiplyBy cur acc) (getDigits 3628800)
    |> List.sum
    |> printfn "%i"

    0