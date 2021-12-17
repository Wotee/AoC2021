let input = System.IO.File.ReadAllLines("inputs/day14.txt")

#time

let rules =
    input
    |> Array.skip 2
    |> Array.map (fun line -> line.Split([|" -> "|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun x -> (x.[0].[0], x.[0].[1]), char x.[1] )
    |> Map.ofArray

let repeat n f =
    Array.init n (fun _ -> f) |> Array.reduce (>>)

let initialState : Map<char*char, int64> = 
    input.[0]
    |> Seq.pairwise
    |> Seq.countBy id 
    |> Seq.map (fun (x, y) -> x, int64 y)
    |> Map.ofSeq

let round (state : Map<(char*char), int64>) = 
    state
    |> Seq.collect (fun (KeyValue ((f, s), n)) ->
        let x = rules[f,s]
        [(f, x), n; (x, s), n])
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k, v |> Seq.sumBy snd)
    |> Map.ofSeq

let countChars (map : Map<char*char, int64>) =
    map
    |> Map.fold (fun (acc : Map<char,int64>) (k, _) n ->
        acc |> Map.change k (function None -> Some n | Some x -> Some (x + n))
    ) ([input.[0] |> Seq.last, 1L] |> Map.ofList) // Add the last character here once, since we gather only first characters of the pairs

let solve n = 
    initialState
    |> repeat n round
    |> countChars
    |> Map.toSeq
    |> fun values ->
        let min = values |> Seq.minBy snd |> snd
        let max = values |> Seq.maxBy snd |> snd
        max - min

[|10; 40|]
|> Array.iteri (fun i n -> solve n |> printfn "Part %i: %i" (i+1))