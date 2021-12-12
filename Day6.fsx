let input = System.IO.File.ReadAllText("inputs/day6.txt").Split(",") |> Array.map int

#time
let state =
    input
    |> Array.countBy id
    |> Array.fold (fun acc (key, count) -> acc |> Map.add key (int64 count)
    ) (Array.init 9 (fun i -> i, 0L) |> Map.ofArray)

let step (map : Map<int, int64>) =
    map 
    |> Map.fold (fun acc key value -> acc |> Map.add (key-1) value) Map.empty
    |> Map.change 6 (Option.map ((+) map.[0])) // Old fish reset to 6
    |> Map.add 8 map.[0] // New fist start at 8
    |> Map.remove -1 // These are reseted

let repeat n f = 
    Array.init n (fun _ -> f)
    |> Array.reduce (>>)

[|80;256|]
|> Array.iteri (fun i n ->
    state
    |> repeat n step
    |> Seq.sumBy ((|KeyValue|) >> snd)
    |> printfn "Part %i: %i" (i+1)
)