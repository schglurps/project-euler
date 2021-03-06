﻿// Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
// 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
// By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

[<EntryPoint>]
let main argv =
    (0, 1)
    |> Seq.unfold (fun state ->
        let (first, second) = state
        Some(first + second, (second, first + second)))
    |> Seq.takeWhile (fun i -> i < 4000000)
    |> Seq.filter (fun i -> i % 2 = 0)
    |> Seq.sum
    |> printfn "%i"

    0