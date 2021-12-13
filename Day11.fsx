module Dictionary = 
    let mapValue f = 
        Seq.map (fun (KeyValue (k, v)) -> (k, f v)) >> dict
    let map f = 
        Seq.map f >> dict
    let mapSingle key f = 
        Seq.map (fun (KeyValue (k, v)) -> if k = key then k, f v else k, v) >> dict

#time
// let input = System.IO.File.ReadAllLines("inputs/day11-sample.txt")
let input = System.IO.File.ReadAllLines("inputs/day11.txt")

type Dictionary = System.Collections.Generic.IDictionary<int*int, int option>

let mutable flashCount = 0

let data : Dictionary =
    input
    |> Array.mapi (fun i line -> line |> Seq.mapi (fun j c -> (i, j), Some ((string >> int) c)))
    |> Array.collect Seq.toArray
    |> dict

let getAdjacentCoordinates x y : (int*int) array = 
    Array.allPairs [|-1;0;1|] [|-1; 0; 1|]
    |> Array.filter ((<>) (0,0))
    |> Array.map (fun (dx, dy) -> x+dx, y+dy)

let findFlashing (dict : Dictionary) = 
    dict
    |> Seq.choose (fun (KeyValue (k, v)) -> match v with | Some x when x > 9 -> Some k | _ -> None)
    |> Seq.toArray

let round map = 
    let rec flashRound (map' : Dictionary) =
        match map' |> findFlashing with
        | [||] -> map'
        | flashing -> 
            flashCount <- flashCount + flashing.Length
            let flashedMap = flashing |> Array.fold (fun acc coords -> acc |> Dictionary.mapSingle coords (fun _ -> None)) map'
            let afterFlashMap =
                flashing 
                |> Array.collect (fun (x,y) -> getAdjacentCoordinates x y)
                |> Array.fold (fun (acc : Dictionary) coords -> acc |> Dictionary.mapSingle coords (Option.map ((+) 1))
                ) flashedMap
            flashRound afterFlashMap
    // First, set all previously flased ones to zero, and raise all energy levels by one
    map
    |> Dictionary.mapValue (function Some x -> Some (x+1) | None -> Some 1)
    // Then do all the necessary flashrounds
    |> flashRound

let repeat n f = 
    List.init n (fun _ -> f) |> List.reduce (>>)

data
|> repeat 100 round

printfn "%A" flashCount

