let input = System.IO.File.ReadAllLines("input/day10.txt")

#time

let (|CloseBracket|_|) = function
    | ')' | '>' | '}' | ']' as cb -> Some (CloseBracket cb)
    | _ -> None

let matches x y =
    match y, x with
    | '(', ')' | '<', '>' | '[', ']' | '{', '}' -> true
    | _, _ -> false

let toPoints = function
    | ')' -> 3L
    | ']' -> 57L
    | '}' -> 1197L
    | '>' -> 25137L
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | _ -> 4L

let parseUntilError (v : string) =
    let rec parse =  function
        | [], s -> s, None
        | CloseBracket cb::rest, stackHead::stackTail when stackHead |> matches cb -> parse (rest, stackTail)
        | CloseBracket cb::_, _ -> [], Some cb
        | ob::rest, s -> parse (rest, (ob::s))
    parse ((Seq.toList v), [])

let lines =
    input
    |> Array.map parseUntilError

lines
|> Array.sumBy (snd >> Option.map toPoints >> Option.defaultValue 0)
|> printfn "Part 1: %i"

lines
|> Array.filter (snd >> Option.isNone)
|> Array.map (fun (a, _) -> a |> Seq.fold (fun (acc : int64) (elem : char) -> acc * 5L + toPoints elem) 0L)
|> Array.sort
|> fun arr -> arr.[arr.Length/2]
|> printfn "Part 2: %i"