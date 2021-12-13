let input = System.IO.File.ReadAllText("inputs/day6.txt").Split(",") |> Array.map int |> Array.countBy id |> Map.ofArray

#time

let intialState = Array.init 9 (input.TryFind >> Option.map int64 >> Option.defaultValue 0)

let repeat n f = 
    Array.init n (fun _ -> f) |> Array.reduce (>>)

let step state =
    let newState = [|yield! Array.tail state; yield Array.head state|]
    newState[6] <- newState[6] + state[0]
    newState

[|80;256|]
|> Array.iteri (fun i n ->
    intialState
    |> repeat n step
    |> Array.sum
    |> printfn "Part %i: %i" (i+1)
)