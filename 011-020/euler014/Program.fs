// The following iterative sequence is defined for the set of positive integers:

// n → n/2 (n is even)
// n → 3n + 1 (n is odd)

// Using the rule above and starting with 13, we generate the following sequence:
// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

// It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem),
// it is thought that all starting numbers finish at 1.

// Which starting number, under one million, produces the longest chain?

// NOTE: Once the chain starts the terms are allowed to go above one million.

open System.Collections.Generic

[<EntryPoint>]
let main argv =

    let rec collatzLength =
        let cache = new Dictionary<int64, int>()

        let rec innerCollatzLength n =
            let (found, length) = cache.TryGetValue n
            if found then
                length
            else
                match n with
                | 1L -> 1
                | n when n % 2L = 0L ->
                    let result = 1 + (innerCollatzLength (n / 2L))
                    cache.Add(n, result)
                    result
                | n ->
                    let result = 1 + (innerCollatzLength (3L * n + 1L))
                    cache.Add(n, result)
                    result

        innerCollatzLength

    seq { 1L .. 999999L }
    |> Seq.maxBy collatzLength
    |> printfn "%i"

    0