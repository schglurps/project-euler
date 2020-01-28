// A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are
// listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

// 012   021   102   120   201   210

// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

open System.Collections.Generic

let c = new List<string>()

let print (arr: int array) =
    arr
    |> Array.fold (fun acc cur -> sprintf "%s%i" acc cur ) ""
    |> c.Add

let swap (arr: int array) i j =
    let temp = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- temp


let rec heap (arr: int array) (size: int) (n: int) =  
    if size = 1 then
        print arr
    else
        seq { 0..size - 1 }
        |> Seq.iter (fun i ->
            heap arr (size - 1) n
            if size % 2 = 1 then
                swap arr 0 (size-1)
            else
                swap arr i (size-1))


[<EntryPoint>]
let main argv =
    let arr =  [| 0; 1; 2; 3;4; 5; 6; 7; 8; 9 |]
    heap arr 10 10
    c.Sort()
    printfn "%s" c.[999999]
    0