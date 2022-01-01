#time
let input =
    System.IO.File.ReadAllLines("inputs/day21.txt")
    |> Array.map (Seq.last >> string >> int)

let position p rolls =
    match (p + rolls) % 10 with
    | 0 -> 10
    | x -> x

let play p1Pos p2Pos = 
    let rec turn pos otherPos score otherScore rolls = 
        match otherScore >= 1000 with
        | true -> score * rolls
        | false ->
            let newPos = position pos (6 + rolls * 3)
            turn otherPos newPos otherScore (score + newPos) (rolls + 3)
    turn p1Pos p2Pos 0 0 0 

play input[0] input[1]
|> printfn "Part 1: %i"

let diracDice = 
    let r = [|1;2;3|]
    Array.allPairs r r 
    |> Array.allPairs r
    |> Array.map (fun (x, (y, z)) -> x + y + z)
    |> Array.countBy id

let memoize fn = 
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    fun key ->
        match cache.TryGetValue key with
        | true, v -> v
        | false, _ ->
            let v = fn key
            cache.Add(key, v)
            v

let rec round (pos, otherPos, score, otherScore) =
    let countWin (wins1, wins2) (total, count) = 
        let newPos = position pos total
        let newScore = score + newPos
        let w2, w1 = memoizedRound(otherPos, newPos, otherScore, newScore)
        wins1 + w1 * (int64 count), wins2 + w2 * (int64 count)
    match otherScore >= 21 with
    | true -> 0L, 1L
    | false -> Array.fold countWin (0, 0) diracDice
and memoizedRound = memoize round

let playWithDiracDice p1 p2 =
    memoizedRound (p1, p2, 0, 0)
    |> fun (x, y) -> max x y

playWithDiracDice input[0] input[1]
|> printfn "Part 2: %i"