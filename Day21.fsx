#time
let input = System.IO.File.ReadAllLines("inputs/day21.txt") |> Array.map (Seq.last >> string >> int)

let rollDeterministic =
    let mutable result = 0
    fun () ->
        result <- result + 1
        if result > 100 then result <- 1
        result

let position (pos: int) (rolls: int array) =
    match (Array.sum rolls + pos) % 10 with
    | 0 -> 10
    | x -> x

let game p1StartPos p2Startpos =
    ((p1StartPos, 0), (p2Startpos, 0))
    |> Seq.unfold (fun ((p1Pos, p1Points), (p2Pos, p2Points)) ->
        if p1Points >= 1000 || p2Points >= 1000 then
            None
        else
            let newP1Pos = position p1Pos (Array.init 3 (fun _ -> rollDeterministic ()))
            let newP1Points = p1Points + newP1Pos
            let newP2Pos = position p2Pos (Array.init 3 (fun _ -> rollDeterministic ()))
            let newP2Points = p2Points + newP2Pos
            Some ((p1Points, p2Points), ((newP1Pos, newP1Points), (newP2Pos, newP2Points)))
    ) |> Seq.cache

let getAnswer (res : (int*int) seq) =
    let len = Seq.length res * 6
    match Seq.last res with
    | (p1, p2) when p1 < p2 -> p1 * len
    | (_, p2) -> p2 * (len - 3) // Second player didn't roll

game input[0] input[1]
|> getAnswer
|> printfn "Part 1: %i"