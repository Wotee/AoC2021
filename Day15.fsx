open System.Collections.Generic
let input = System.IO.File.ReadAllLines("inputs/day15.txt")

#time

type Node = 
    {  distance : int
       weight : int }

module Node =
    let create weight =
        { distance = System.Int32.MaxValue; weight = (string >> int) weight }
    let update node = 
        { node with weight = match node.weight with | 9 -> 1 | x -> x + 1}

let getNeighborCoordinates (x, y) =
    [|x, y-1; x, y+1; x-1, y; x+1, y|]

let repeat n f = 
    match n with
    | 0 -> id
    | _ -> Array.init n (fun _ -> f) |> Array.reduce (>>)

let mapOriginalToNewRows (input : string array) = 
    let mapSingleLine line = 
        Array.init 5 (fun times -> line |> Seq.toArray |> Array.map (string >> int >> Node.create >> (repeat times Node.update)))
        |> Array.concat
    Array.init (Array.length input) (fun n -> mapSingleLine input.[n])

let newRowsToWholeMap (input : Node[][]) =
    [| for n in [0..4] do for row in input -> row |> Array.map (repeat n Node.update) |]

let getPart2Map input =
    let map = 
        input
        |> mapOriginalToNewRows
        |> newRowsToWholeMap
        |> array2D
    map[0, 0] <- { map[0, 0] with distance = 0 }
    map

let getPart1Map input = 
    let map =
        input
        |> Array.map (Seq.toArray >> Array.map Node.create)
        |> array2D
    map[0, 0] <- { map[0, 0] with distance = 0 }
    map

let dijkstra start (graph : Node [,]) = 
    let mutable pq = PriorityQueue<int*int, int>()
    let maxX = (graph |> Array2D.length1) - 1
    let maxY = (graph |> Array2D.length2) - 1
    pq.Enqueue(start, 0)
    let mutable coords = (-1,-1)
    let mutable distance = -1
    while pq.TryDequeue(&coords, &distance) do
        let neihgborCoords = coords |> getNeighborCoordinates |> Array.filter (fun (x,y) -> 0 <= x && x <= maxX && 0 <= y && y <= maxY)
        for (nx, ny) in neihgborCoords do
            let newDist = distance + graph[nx, ny].weight
            if newDist < graph[nx, ny].distance && newDist > 0 then
                graph[nx, ny] <- { graph[nx, ny] with distance = newDist }
                pq.Enqueue((nx,ny), newDist)
    graph[maxX, maxY]

[|getPart1Map; getPart2Map|]
|> Array.iteri (fun i f -> printfn "Part %i: %i" (i+1) (input |> f |> dijkstra (0,0)).distance)