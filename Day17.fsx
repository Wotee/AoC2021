#time
let input = System.IO.File.ReadAllText("inputs/day17.txt")
let values = input.Split([|"target area: x="; ", y="; ".."|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
let xs, ys = values[..1], values[2..]

// Min y-velocity to launch is min ys, because smaller would never hit the area.
// Then max y-velocity v is the value where the probe hits y=0 again with min y-velocity
// => min_y = -(v+1) => v = -min_y - 1
let minY, maxY = Array.min ys, Array.max ys
let minX, maxX = Array.min xs, Array.max xs

let max_v_y = -minY - 1
max_v_y*(max_v_y + 1)/2 |> printfn "Part 1: %i"

let reverseCombinatorial x =
    let fx = float x
    let result = 0.5*((System.Math.Sqrt(8.*fx+1.))-1.)
    System.Math.Ceiling result |> int

let possibleYs = [minY..max_v_y]
let min_v_x = reverseCombinatorial minX // Slower than this would never reach target xs
let possibleXs = [min_v_x..(maxX / 2)] @ [minX..maxX]

let towardsZero x = if x > 0 then x - 1 else 0

let isInTarget (x, y) = 
    minX <= x && x <= maxX && minY <= y && y <= maxY

let trajectoryHitsTarget (v_x_start,v_y_start) =
    (0, 0, v_x_start, v_y_start)
    |> Seq.unfold (fun ((xPos, yPos, v_x, v_y)) ->
        if xPos > maxX || yPos < minY then None
        else Some ((xPos, yPos), (xPos + v_x, yPos + v_y, towardsZero v_x, v_y - 1))) 
    |> Seq.exists isInTarget

List.allPairs possibleXs possibleYs
|> Seq.filter trajectoryHitsTarget 
|> Seq.length 
|> printfn "Part 2: %i"