#time
let input = System.IO.File.ReadAllLines("inputs/day18.txt")

type Number = 
    | Regular of int
    | Pair of Number * Number
with override x.ToString() = 
        match x with
        | Regular d -> $"{d}"
        | Pair (l, r) -> $"[{l},{r}]"

type Dir = L | R
module Number = 
    let private splitByTopLevelComma (x : string) =
        let rec getIndex rest counter = 
            match rest |> Seq.head with
            | _, '[' -> getIndex (rest |> Seq.tail) (counter + 1)
            | _, ']' -> getIndex (rest |> Seq.tail) (counter - 1)
            | index, ',' when counter = 0 -> index
            | _ -> getIndex (rest |> Seq.tail) counter
        let index = getIndex (Seq.indexed x) 0
        x[..index-1], x[index+1..]

    let rec fromString (x : string) = 
        match x[0] with
        | '[' ->
            let left, right = x[1..^1] |> splitByTopLevelComma
            Pair (fromString left, fromString right)
        | _ -> Regular (int x)

    let private explode (x : Number) =
        let rec add dir n v = 
            match n, dir with
            | Regular d, _ -> Regular (d + v)
            | Pair (l, r), L -> Pair (add L l v, r)
            | Pair (l, r), R -> Pair (l, add R r v)

        let rec goDeeper node depth = 
            match node with
            | Regular _ -> node, None
            | Pair (Regular l, Regular r) when depth >= 4 ->
                Regular 0, Some (l,r)
            | Pair (l, r) ->
                let newL, newR, lAdd = 
                    match goDeeper l (depth + 1) with
                    | newL, Some (explodedL, explodedR) -> newL, add L r explodedR, Some explodedL
                    | _ -> l, r, None

                let newL, newR, rAdd =
                    match goDeeper newR (depth + 1) with
                    | newR, Some (explodedL, explodedR) -> add R newL explodedL, newR, Some explodedR
                    | _ -> newL, newR, None

                match lAdd, rAdd with
                | None, None -> Pair(newL, newR), None
                | _ -> Pair(newL, newR), Some(Option.defaultValue 0 lAdd, Option.defaultValue 0 rAdd)
        goDeeper x 0 |> fst

    let private split (x : Number) = 
        let rec goDeeper (x : Number) = 
            match x with
            | Regular number when number >= 10 ->
                let result = float number / 2.
                let lower, upper = floor result, ceil result
                Pair (Regular (int lower), Regular (int upper)), true
            | Regular _ as r -> r, false
            | Pair (l, r) ->
                match goDeeper l with
                | splitL, true -> Pair(splitL, r), true
                | l, false ->
                    match goDeeper r with
                    | splitR, true -> Pair(l, splitR), true
                    | r, false -> Pair(l,r), false
        goDeeper x

    let rec reduce (x : Number) = 
        match x |> explode |> split with
        | afterSplit, true -> reduce afterSplit
        | noSplit, false -> noSplit

    let sum =
        Array.reduce (fun acc elem ->
            $"[{acc},{elem}]"
            |> fromString
            |> reduce
            |> sprintf "%O")
        >> fromString

    let rec magnitude (x : Number) =
        match x with
        | Regular r -> r
        | Pair (l, r) ->
            3 * (magnitude l) + 2 * (magnitude r)

input |> Number.sum |> Number.magnitude |> printfn "Part 1: %i"