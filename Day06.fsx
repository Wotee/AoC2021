#time
let input =
    System.IO.File.ReadAllText("inputs/day6.txt").Split(",")
    |> Array.map int
    |> Array.countBy id
    |> Map.ofArray

let intialState =
    Array.init 9 (input.TryFind >> Option.map int64 >> Option.defaultValue 0)

let simulation = 
    intialState
    |> Seq.unfold (fun arr ->
        let newState = [|yield! Array.tail arr; yield Array.head arr|]
        newState[6] <- newState[6] + arr[0]
        Some (Array.sum newState, newState)) 
    |> Seq.cache

simulation |> Seq.item 79 |> printfn "Part 1: %i"
simulation |> Seq.item 255 |> printfn "Part 2: %i"