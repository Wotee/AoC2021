let input = System.IO.File.ReadAllLines("inputs/day1.txt") |> Array.map int

#time

[|1;3|]
|> Array.iteri (fun i window ->
    Array.foldBack2 (fun x y -> (+) (System.Convert.ToInt32(x<y))) input[..^window] input[window..] 0
    |> printfn "Part %i: %i" (i+1))