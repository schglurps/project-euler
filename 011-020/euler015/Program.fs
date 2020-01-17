// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
// How many such routes are there through a 20×20 grid?

open System.Collections.Generic

[<EntryPoint>]
let main argv =
 
    let cache = new Dictionary<int * int, int64>()
    cache.Add ((0, 0), 0L)

    let rec countPaths point = 
        let (found, count) = cache.TryGetValue point
        if found then
            count
        else
            let (x, y) = point
            if x = 0 || y = 0 then
                1L
            else
                let paths = countPaths (x, y - 1) + countPaths (x- 1, y)
                cache.Add (point, paths)
                paths

    printfn "%i" (countPaths (20, 20))
    0