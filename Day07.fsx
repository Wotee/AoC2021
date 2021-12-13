let input =
    System.IO.File.ReadAllText("./input/day7.txt").Split(',') |> Array.map float |> Array.sort

#time

let hackyMedian (sorted : float array)  = 
   sorted[sorted.Length/2]

let part1BestPosition = input |> hackyMedian
let part2BestPosition = (input |> Array.average |> System.Math.Round) - 1.

input |> Array.sumBy ((-) part1BestPosition >> abs) |> printfn "Part 1: %.0f"
input |> Array.sumBy ((-) part2BestPosition >> abs >> fun n -> 0.5*n*(n+1.)) |> printfn "Part 2: %.0f"