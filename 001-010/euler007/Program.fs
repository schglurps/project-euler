// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?

[<EntryPoint>]
let main argv =

    let primes limit =
        let flags = Array.create (limit - 1) true // [| 2.. limit |]
        let markNumber i = flags.[i - 2] <- false

        let mutable current = Some 2
        let s = limit |> float |> sqrt |> int

        while current.IsSome && current.Value <= s do
            Seq.unfold (fun i -> Some(i, i + current.Value)) (current.Value * current.Value)
            |> Seq.takeWhile (fun i -> i <= limit)
            |> Seq.iter markNumber

            let index =
                seq { current.Value + 1 .. (limit - 1)  }
                |> Seq.tryFindIndex (fun i -> flags.[i - 2])

            current <-
                match index with
                | Some x -> Some (x + current.Value + 1)
                | _ -> None

        seq {
            for i in 0 .. (limit - 2) do
                if flags.[i] then yield i + 2
        }

    primes 105000
    |> Seq.skip 10000
    |> Seq.head
    |> printfn "%i"

    0