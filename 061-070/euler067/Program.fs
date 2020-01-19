// By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

// 3
// 7 4
// 2 4 6
// 8 5 9 3

// That is, 3 + 7 + 4 + 9 = 23.

// Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

// NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 2^99 altogether!
// If you could check one trillion (10^12) routes every second it would take over twenty billion years to check them all.
// There is an efficient algorithm to solve it. ;o)

open System.IO

[<EntryPoint>]
let main argv =
    let triangle = 
        File.ReadLines "input.txt"
        |> Seq.map (fun line -> 
            line.Split ' '
            |> Array.map int)
        |> List.ofSeq

    triangle
    |> List.reduce (fun acc cur ->
        cur
        |> Array.mapi (fun i x ->
            let a = if i = 0 then 0 else acc.[i - 1]
            let b = if i = acc.Length then 0 else acc.[i]
            x + (max a b)
        ))
    |> Array.max
    |> printfn "%i"

    0
