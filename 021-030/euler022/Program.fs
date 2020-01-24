// Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order.
// Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

// For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
// So, COLIN would obtain a score of 938 × 53 = 49714.

// What is the total of all the name scores in the file?

open System.IO

[<EntryPoint>]
let main argv =

    let a = int 'A' 
    let position character = int character - a + 1

    use sr = new StreamReader("input.txt")
    let line = sr.ReadLine()
    let arr = line.Split(',')
    arr |> Array.sortInPlace |> ignore
    arr
    |> Array.mapi (fun i quotedName ->
        let name = quotedName.Substring(1, quotedName.Length - 2)
        let nth = i + 1
        let sum =  name.ToCharArray() |> Array.sumBy position
        nth * sum)
    |> Array.sum
    |> printfn "%i"

    0