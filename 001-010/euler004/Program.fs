// A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
// Find the largest palindrome made from the product of two 3-digit numbers.

[<EntryPoint>]
let main argv =

    let isPalindrome number =
        let reversed =
            number
            |> Seq.unfold (fun digit ->
                if digit = 0 then
                    None
                else
                    let modulo = digit % 10
                    let r = digit / 10
                    Some(modulo, r))
            |> Seq.fold (fun state current -> 10 * state + current) 0

        number = reversed

    seq {
        for i in 999 .. -1 .. 100 do
            for j in i .. -1 .. 100 do
                i * j
    }
    |> Seq.filter isPalindrome
    |> Seq.max
    |> printfn "%i"

    0