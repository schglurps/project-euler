// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

// What is the sum of the digits of the number 2^1000?

[<EntryPoint>]
let main argv =

    let multiplyBy2 (digits: int list) =
        let rec innerMul (acc: int list) (innerDigits: int list) remainder =
            match innerDigits with
            | x::xs -> 
                let r = x * 2 + remainder
                let newDigit = r % 10
                let newRemainder = r / 10
                innerMul (newDigit::acc) xs newRemainder
            | [] ->
                if remainder = 0 then
                    acc
                else
                    remainder::acc

        innerMul [] digits 0
        |> List.rev

    { 2 .. 1000 }
    |> Seq.fold (fun acc _ -> multiplyBy2 acc ) [2]
    |> List.sum
    |> printfn "%i"

    0