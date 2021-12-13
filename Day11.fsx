module Dictionary = 
    let mapValue f = 
        Seq.map (fun (KeyValue (k, v)) -> (k, f v)) >> dict
    let map f = 
        Seq.map f >> dict
    let mapSingle key f = 
        Seq.map (fun (KeyValue (k, v)) -> if k = key then k, f v else k, v) >> dict

let input = System.IO.File.ReadAllLines("input/day11-sample.txt")
// let input = System.IO.File.ReadAllLines("input/day11.txt")
type Dictionary = System.Collections.Generic.IDictionary<int*int, int option>

let data : Dictionary =
    input
    |> Array.mapi (fun i line -> line |> Seq.mapi (fun j c -> (i, j), Some ((string >> int) c)))
    |> Array.collect Seq.toArray
    |> dict

let getAdjacentCoordinates x y : (int*int) list= 
    List.allPairs [-1;0;1] [-1; 0; 1]
    |> List.filter ((<>) (0,0))
    |> List.map (fun (dx, dy) -> x+dx, y+dy)

let getFlashed (dict : Dictionary) = 
    dict
    |> Seq.filter (fun (KeyValue (k, v)) -> match v with | Some x when x > 9 -> true | _ -> false)
    |> Seq.map (fun (KeyValue (x, y)) -> x, None)
    |> Seq.toArray

let doTheFlashyThing (map : Dictionary) : Dictionary * (int*int) list =
    let theOnes =
        map
        |> Seq.filter (fun (KeyValue (key, value)) -> match value with | Some x when x > 9 -> true > 9 )
        |> Seq.toArray
        |> Array.map getAdjacentCoordinates


let round map =
    let rec flashRound (map' : Dictionary) (toBeFlashed : (int*int) array) =
        toBeFlashed
        |> Array.fold (fun (acc : Dictionary) coords -> acc |> Dictionary.mapSingle coords (Option.map ((+) 1))
        ) map'
        |> doTheFlashyThing
        
        // |> Array.fold (fun (dictionary, newOnes) coords ->
        //     dictionary
        //     |> Dictionary.mapSingle coords (fun value -> value |> Option.map ((+) 1))
        // ) (map', [||])

        // |> fun x ->
        //     getFlashed x
        //     |> function
        //     | [||] -> x
        //     | rest ->
        //         printfn "%A" rest
        //         flashRound x (rest |> Array.map fst)
    // First, set all previously flashed ones to zero, and raise all energy levels by one
    let newMap = 
        map
        |> Dictionary.mapValue (function Some x -> Some (x+1) | None -> Some 1)
    newMap
    |> getFlashed
    |> Array.map fst
    |> flashRound newMap

data
|> round
|> round
|> Seq.map (fun (KeyValue (_,value)) -> value)
|> Seq.map (Option.defaultValue 0)
|> Seq.chunkBySize 10
|> Seq.toArray
|> array2D
