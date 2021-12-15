// let input = System.IO.File.ReadAllLines("inputs/day14-sample.txt")
let input = System.IO.File.ReadAllLines("inputs/day14.txt")
let start = input.[0]

let rules =
    input 
    |> Array.skip 2 
    |> Array.map (fun line -> line.Split([|" -> "|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> (x.[0].[0], x.[0].[1]), char x.[1] )
    |> Map.ofArray

let round state = 
    state
    |> Seq.pairwise
    |> Seq.collect (fun key ->
        match rules |> Map.tryFind key with
        | Some value -> [fst key; value]
        | None -> [fst key]) 
    |> System.String.Concat
    |> fun x -> $"{x}{state |> Seq.last}"

let repeat n f =
    Array.init n (fun _ -> f) |> Array.reduce (>>)

start
|> repeat 10 round
|> Seq.countBy id
|> fun values ->
    let min = values |> Seq.minBy snd |> snd
    let max = values |> Seq.maxBy snd |> snd
    max - min
|> printfn "Part 1: %i"