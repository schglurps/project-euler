// The following iterative sequence is defined for the set of positive integers:

// n → n/2 (n is even)
// n → 3n + 1 (n is odd)

// Using the rule above and starting with 13, we generate the following sequence:
// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

// It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem),
// it is thought that all starting numbers finish at 1.

// Which starting number, under one million, produces the longest chain?

// NOTE: Once the chain starts the terms are allowed to go above one million.

[<EntryPoint>]
let main argv =

    let rec collatzLength (n: int64) =
        match n with
        | 1L -> 1
        | n when n % 2L = 0L -> 1 + (collatzLength (n / 2L))
        | n -> 1 + (collatzLength (3L * n + 1L))

    seq { 1L .. 999999L }
    |> Seq.maxBy collatzLength
    |> printfn "%i"

    0