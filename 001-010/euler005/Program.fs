// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

[<EntryPoint>]
let main argv =

    let lcm (a: int64) (b: int64) =
        let rec gcd (dividend: int64) (divisor: int64) =
            let remainder = dividend % divisor
            if remainder = 0L then
                divisor
            else
                gcd divisor remainder
        
        let g = gcd (max a b) (min a b)
        (a * b) / g

    [2L..20L]
    |> List.reduce lcm
    |> printfn "%i"

    0