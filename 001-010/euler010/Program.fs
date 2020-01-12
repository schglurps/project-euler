// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
// Find the sum of all the primes below two million.

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
                if flags.[i] then yield int64(i + 2)
        }

    primes 2000000
    |> Seq.sum
    |> printfn "%i"
    
    0