// A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
// a^2 + b^2 = c^2

// For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.

[<EntryPoint>]
let main argv =
    let (i, j, k) =
        seq {
            for i in 1..997 do
                for j in (i + 1) .. 998 do
                    yield (i, j, 1000 - i - j)
        }
        |> Seq.find (fun t ->
            let (i, j, k) = t
            i * i + j * j = k * k)

    printfn "%i" (i * j * k)
    0