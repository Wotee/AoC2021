let input = System.IO.File.ReadAllLines("inputs/day12.txt")

#time
type Cave =
| Small of string
| Large of string

module Cave =
    let fromString (x : string) = 
        if System.Char.IsUpper x.[0] then Large x else Small x

type CaveMap = Map<Cave,Set<Cave>>
module CaveMap =
    let add (x : Cave) (y : Cave) (map : CaveMap) =
        map
        |> Map.change x (Option.map (Set.add y) >> Option.defaultValue (set [y]) >> Some)
        |> Map.change y (Option.map (Set.add x) >> Option.defaultValue (set [x]) >> Some)

let map = 
    input
    |> Array.fold (fun map (line : string) ->
        let caves = 
            line.Split("-")
            |> Array.map Cave.fromString
        map |> CaveMap.add caves.[0] caves.[1]
    ) Map.empty

let getRoutes (map : CaveMap) allowDoubleVisit = 
    let rec getPossibleRoutes (step : Cave) currentRoute smallVisitedTwice = 
        seq {
            let newRoute = currentRoute @ [step]
            match step with
            | Small "end" -> yield newRoute // end always terminates
            | step ->
                let visitedSmallCaves = newRoute |> List.filter (function Small _ -> true | _ -> false)
                let visited = smallVisitedTwice || (visitedSmallCaves |> List.length) <> (visitedSmallCaves |> Set.ofList |> Set.count)

                let possibleNextSteps = 
                    map.[step]
                    |> Set.filter ((<>) (Small "start"))
                    |> Set.filter (if visited then (fun newStep -> visitedSmallCaves |> List.contains newStep |> not) else fun _ -> true)

                for step in possibleNextSteps do yield! getPossibleRoutes step newRoute visited
        }
    getPossibleRoutes (Small "start") List.empty (not allowDoubleVisit)

[|false; true|] |> Array.iteri (fun i x -> getRoutes map x |> Seq.length |> printfn "Part %i: %i" (i+1))