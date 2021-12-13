let input =
    System.IO.File.ReadAllLines("inputs/day5.txt")
    |> Array.map (fun x -> x.Split([|","; " -> "|], System.StringSplitOptions.None) |> Array.map int)

#time

let countOverlapping passThrough = 
    let step x y = if x < y then 1 else -1
    if passThrough then id else Array.filter (fun [|x1;y1;x2;y2|] -> x1 = x2 || y1 = y2)
    >> Array.collect (fun ([|x1;y1;x2;y2|] : int array) ->
        ([|x1..step x1 x2..x2|], [|y1..step y1 y2..y2|])
        ||> if x1 = x2 || y1 = y2 then Array.allPairs else Array.zip)
    >> Array.countBy id
    >> Array.sumBy (fun (_,value) -> System.Convert.ToInt32(value > 1))

[|false;true|]
|> Array.iteri (fun i passThrough ->
    countOverlapping passThrough input
    |> printfn "Part %i: %i" (i+1)
)