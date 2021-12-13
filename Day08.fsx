let input = System.IO.File.ReadAllLines("inputs/day8.txt")

#time

input
|> Array.collect (fun x -> x.Split('|').[1].Split(" ", System.StringSplitOptions.RemoveEmptyEntries))
|> Array.sumBy(String.length >> function | 2 | 3 | 4 | 7 -> 1 | _ -> 0)
|> printfn "Part 1: %i"

let intersect x y =
    Set.intersect (Set.ofSeq x) (Set.ofSeq y)
    |> Set.count

let identifiers : ((string -> string -> string-> bool)*string) array =
    [|
        (fun _ _ s -> s.Length = 2), "1"
        (fun _ _ s -> s.Length = 4), "4"
        (fun _ _ s -> s.Length = 3), "7"
        (fun _ _ s -> s.Length = 7), "8"
        (fun _ four s -> s.Length = 5 && intersect s four = 2), "2"
        (fun one _ s -> s.Length = 5 && intersect s one = 2), "3"
        (fun _ four s -> s.Length = 5 && intersect s four = 3), "5"
        (fun _ four s -> s.Length = 6 && intersect s four = 4), "9"
        (fun one four s -> s.Length = 6 && intersect s one = 1 && intersect s four = 3), "6"
        (fun _ _ _ -> true), "0" // Last one to check, must be it
    |]

let decode (line : string) =
    let split = line.Split('|') |> Array.map (fun x -> x.Split(" ", System.StringSplitOptions.RemoveEmptyEntries))
    let both = split |> Array.collect id
    let one = both |> Array.find (String.length >> (=) 2)
    let four = both |> Array.find (String.length >> (=) 4)
    split.[1]
    |> Array.map (fun number -> identifiers |> Array.find (fun (matcher, _) -> matcher one four number) |> snd)
    |> System.String.Concat
    |> int

input
|> Array.sumBy decode
|> printfn "Part 2: %i"