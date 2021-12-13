// let input = System.IO.File.ReadAllLines("input/day9-sample.txt")
let input = System.IO.File.ReadAllLines("input/day9.txt")

let values = 
    input
    |> Array.map(Seq.toArray)
    |> array2D
    |> Array2D.map (string >> int)

let maxX = Array2D.length2 values - 1
let maxY = Array2D.length1 values - 1

let adjacentCoordinates y x =
    [|y, x-1; y, x+1; y-1, x; y+1, x|]
    |> Array.filter (fun (y,x) -> 0 <= x && x <= maxX && 0 <= y && y <= maxY)

let findLowPoints map = 
    map
    |> Array2D.mapi (fun x y value ->
        adjacentCoordinates x y
        |> Array.forall (fun (x1, y1) -> Array2D.get values x1 y1 > value)
        |> function  true -> Some (value + 1, x, y) | false -> None)
    |> Seq.cast<(int*int*int) option>
    |> Seq.choose id

values
|> findLowPoints
|> Seq.sumBy (fun (x,_,_) -> x)
|> printfn "Part 1: %i"

let findBasin (x : int) (y : int) =
    let rec step known rest : Set<int*int> =
        match rest with
        | [] -> known : Set<int*int>
        | (x,y)::tail ->
            let newCoords = 
                adjacentCoordinates x y
                |> Array.filter (fun (x, y) -> not (Set.contains (x, y) known))
                |> Array.filter (fun (x, y) -> Array2D.get values x y < 9)
                |> Array.toList
            let newKnown = newCoords |> Set.ofList |> Set.union known
            step newKnown (tail @ newCoords)
    step Set.empty [(x,y)]
    |> Set.count

values
|> findLowPoints
|> Seq.map (fun (_,x,y) -> findBasin x y)
|> Seq.sortDescending
|> Seq.take 3
|> Seq.reduce (*)
|> printfn "Part 2: %i"