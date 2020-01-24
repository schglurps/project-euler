// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
// For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

// A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis,
// it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by
// analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

[<EntryPoint>]
let main argv =
    let maxValue = 28123

    let divisors n =
        seq {
            yield 1

            let isEven = n % 2 = 0
            if isEven then yield 2

            let candidates =
                if isEven then
                    seq { 3..n/2 }
                else
                    seq { 3..2..n/2 }

            yield! Seq.filter (fun i -> n % i = 0) candidates
        }

    let isAbundant n =
        let sum = divisors n |> Seq.sum
        sum > n

    let abuntantNumbers = 
        seq { 12..maxValue }
        |> Seq.filter isAbundant
        |> List.ofSeq

    let rec getSums list (sums: Set<int>) =
        let rec getSumsForHead head tail (internalSums: Set<int>) = 
            match tail with
            | x::xs ->
                let value = head + x
                let newCache = Set.add value internalSums
                getSumsForHead head xs newCache
            | [] -> internalSums

        match list with
        | x::xs ->
            let sum = x + x
            if sum > maxValue then
                sums
            else
                let updatedSums = getSumsForHead x xs (Set.add sum sums)
                getSums xs updatedSums
        | [] -> sums

    let r = getSums abuntantNumbers Set.empty

    seq { 1..maxValue }
    |> Seq.sumBy (fun i -> if r.Contains i then 0 else i)
    |> printfn "%i"

    0