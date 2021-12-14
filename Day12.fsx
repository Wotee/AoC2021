// let input = System.IO.File.ReadAllLines("inputs/day12-sample.txt")
let input = System.IO.File.ReadAllLines("inputs/day12.txt")

#time
type Cave =
| Small of string
| Large of string

module Cave =
    let fromString (x : string) = 
        if System.Char.IsUpper x.[0]
        then Large x
        else Small x

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

let getRoutes (map : CaveMap) =
    let rec getPossibleRoutes (step : Cave) currentRoute =
        seq {
            let newRoute = currentRoute @ [step]
            match step with
            | Small "end" -> yield newRoute
            | step ->
                let nextSteps = map.[step]
                let visitedSmallCaves = currentRoute |> List.filter (function Small _ -> true | _ -> false)
                let possibleNextSteps = 
                    nextSteps
                    |> Set.filter (fun newStep -> visitedSmallCaves |> List.contains newStep |> not)
                for step in possibleNextSteps do yield! getPossibleRoutes step newRoute
        }
    getPossibleRoutes (Small "start") List.empty

getRoutes map
|> Seq.length
|> printfn "Part 1: %i"