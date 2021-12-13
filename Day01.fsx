let input = System.IO.File.ReadAllLines("inputs/day1.txt") |> Array.map int

#time

[|1;3|]
|> Array.iteri (fun i window ->
    Array.fold2 (fun acc x y -> System.Convert.ToInt32(x<y) + acc) 0 input[..^window] input[window..]
    |> printfn "Part %i: %i" (i+1))