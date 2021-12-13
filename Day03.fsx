let input =
    System.IO.File.ReadAllLines("inputs/day3.txt")
    |> Array.map (seq >> Array.ofSeq)

#time

input
|> Array.transpose
|> Array.map (Array.sort >> fun bits -> bits.[bits.Length / 2])
|> System.String
|> fun s ->
    let x = System.Convert.ToInt32(s,2)
    x * ((2. ** float s.Length |> int) - x - 1)
|> printfn "Part 1: %i"

let parsed = 
    input |> Array.map (Seq.map (string >> int) >> Seq.toArray)

let keep n f (x : int array array, y : int array array) =
    if f x.Length y.Length || x.Length = y.Length && f x.[0].[n] y.[0].[n] then x else y

let findCorrentValues (keepCorrect : int -> int[][]*int[][] -> int[][]) (input : int array array) : int array = 
    let rec round (n : int) : int array array -> int array = function
        | [|x|] -> x
        | inp -> inp |> Array.partition (fun x -> x.[n] = 0) |> keepCorrect n |> round (n+1)
    round 0 input

[|(<);(>)|]
|> Array.fold (fun r f ->
    parsed
    |> findCorrentValues (fun n -> keep n f)
    |> Array.map string
    |> System.String.Concat
    |> fun x -> System.Convert.ToInt32(x, 2)
    |> (*) r
) 1
|> printfn "Part 2: %i"
