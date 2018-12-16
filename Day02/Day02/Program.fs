let makeBag str =
    let mutable bag = Map.empty
    for ch in str do
        if bag.ContainsKey(ch) then
            bag <- bag.Add(ch, (Map.find ch bag) + 1)
        else
            bag <- bag.Add(ch, 1)
    bag

let bagContainsN n bag = 
    (Map.count (Map.filter (fun _ value -> value = n) bag)) > 0

let part1 input =
    let bags = List.map makeBag input
    let twos = List.filter (bagContainsN 2) bags
    let threes = List.filter (bagContainsN 3) bags
    printfn "part1: %A" (twos.Length * threes.Length)

/////////////////////////////////////////////////////////////////
                
let rec countDifferences tally str1 str2 = 
    match str1 with 
    | head1 :: tail1 -> 
        match str2 with 
        | head2 :: tail2 when head1 <> head2 -> countDifferences (tally + 1) tail1 tail2
        | head2 :: tail2 -> countDifferences tally tail1 tail2
        | _ -> invalidOp "lengths of input doesn't match"
    | [] -> tally

let differBy1 (str1 : string) (str2 : string) =
    (countDifferences 0 (List.ofSeq(str1)) (List.ofSeq(str2))) = 1

let rec permutations lst =
    seq {
        match lst with
        | head :: tail -> 
            for item in tail do yield (head, item)
            yield! permutations tail
        | _ -> ignore Seq.empty
    }

let part2 input = 
    let code = Seq.find (fun (x, y) -> differBy1 x y) (permutations input)
    printfn "%A" code

/////////////////////////////////////////////////////////////////

[<EntryPoint>]
let main argv = 
    let input = [for x in System.IO.File.ReadLines("../../input") -> x]
    part2 input
    0