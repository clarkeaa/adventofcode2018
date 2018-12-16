
open System

let rec tally total lst =
    match lst with 
    | [] -> total
    | head :: tail ->
        tally (total + head) tail

let part1 input = 
    printfn "part1 %A" (tally 0 input)

let rec findDupe tally (set : Set<int>) (lst : List<int>) = 
    match lst with
    | head :: tail -> 
        let newSet = set.Add(tally)
        let newTally = tally + head
        if newSet.Contains(newTally) then
            newTally
        else
            findDupe newTally newSet (List.append tail [head])
    | [] -> invalidArg "lst" "empty list supplied"
    
let part2 input = 
    printfn "part2 %A" (findDupe 0 Set.empty input)

[<EntryPoint>]
let main argv = 
    let input = [for x in System.IO.File.ReadLines("../../input") -> Int32.Parse(x)]
    part2 input
    0 // return an integer exit code
