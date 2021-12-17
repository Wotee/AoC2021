let input = System.IO.File.ReadAllLines("inputs/day13.txt")

let splitIndex = input |> Array.findIndex ((=) "")
let pointData, folds = input |> Array.splitAt splitIndex |> fun (x, y) -> x, y |> Array.tail

let points = pointData |> Array.map (fun line -> line.Split(',') |> Array.map int |> fun x -> x.[0], x.[1]) |> Set.ofArray

let fold hinge z = if z < hinge then z else 2*hinge-z
let foldY hinge (x, y) = x, fold hinge y
let foldX hinge (x, y) = fold hinge x, y

let foldInstructions = 
    folds
    |> Array.map (fun line -> line.Replace("fold along ","").Split("=") |> function
        | [|"x"; value|] -> foldX (int value)
        | [|"y"; value|] -> foldY (int value)
        | _ -> failwith "Unexpected input"
    )

points |> Set.map (foldInstructions |> Array.head) |> Set.count |> printfn "Part 1: %i"

let result = 
    foldInstructions
    |> Array.fold (fun acc f -> Set.map f acc) points

let printPart2 points = 
    let maxX = points |> Seq.maxBy fst |> fst |> (+) 1
    let maxY = points |> Seq.maxBy snd |> snd |> (+) 1
    let paper = Array2D.init maxY maxX (fun j i -> match points |> Set.contains (i,j) with | true -> "#" | false -> " ")
    [|0..maxY-1|]
    |> Array.map (fun i -> paper[i, *] |> Seq.cast<string> |> System.String.Concat)
    |> String.concat "\n"

result
|> printPart2
|> printfn "Part 2:\n%s"