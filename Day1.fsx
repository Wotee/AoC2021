let input = System.IO.File.ReadAllLines("inputs/day1.txt") |> Array.map int

[|1;3|]
|> Array.iteri (fun i window ->
    // d[0] + d[1] + d[2] < d[1] + d[2] + d[3] = d[0] < d[3]
    Array.zip input.[..^window] input.[window..]
    |> Array.sumBy (fun (x,y) -> System.Convert.ToInt32(x<y))
    |> printfn "Part %i: %i" (i+1))

