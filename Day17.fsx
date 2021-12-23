// let input = "target area: x=20..30, y=-10..-5"
#time
let input = System.IO.File.ReadAllText("inputs/day17.txt")
let values = input.Split([|"target area: x="; ", y="; ".."|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
let xs, ys = values[..1], values[2..]

// Min y-velocity to launch is min ys, because smaller would never hit the area.
// Then max y-velocity v is the value where the probe hits y=0 again with min y-velocity
// => min_y = -(v+1) => v = -min_y - 1
let v = -(Array.min ys) - 1
v*(v + 1)/2 |> printfn "Part 1: %i"