// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?

open System

[<EntryPoint>]
let main argv =
    let input = 600851475143L

    let divideBy (number: int64) (by: int64) =
        number
        |> Seq.unfold (fun i ->
            if i % by = 0L then
                let n = i / by
                Some (n, n)
            else
                None)
        |> Seq.tryLast

    let largestPrimeFactor (number: int64) = 
        let rec internalLargestPrimeFactor (number: int64) (divisors: seq<int64>) =
            let by = Seq.head divisors
            match by * by with
            | x when x > number -> number
            | x when x = number -> internalLargestPrimeFactor by (Seq.tail divisors)
            | _ ->
                let divided = divideBy number by
                let newNumber =
                    match divided with
                    | Some x -> x
                    | None -> number
                internalLargestPrimeFactor newNumber (Seq.tail divisors)
        
        let divisors =
            2L
            |> Seq.unfold (fun i -> if i = 2L then Some(2L, 3L) else Some(i, i + 2L))
            |> Seq.takeWhile (fun i -> i <= int64(Math.Sqrt(float(number))))
        
        internalLargestPrimeFactor number divisors
    
    printfn "%i" (largestPrimeFactor input)
    0