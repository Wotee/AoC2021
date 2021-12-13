let input = System.IO.File.ReadAllLines("inputs/day4.txt")

#time

let numbers = (Array.head input).Split(',') |> Array.toList

type Board = (string*bool)[,]

let bingoBoards : Board array = 
    input
    |> Array.tail
    |> Array.collect (fun (line : string) -> line.Split(' '))
    |> Array.choose (function "" -> None | x -> Some (x, false))
    |> Array.chunkBySize 25
    |> Array.map (Array.chunkBySize 5 >> array2D)

let markNumber (x : string) (board : Board) = 
    board |> Array2D.map (fun ((n, _) as y) -> if n = x then (x, true) else y)

let sumOfUnmarkeds  = 
    Seq.cast<string*bool> >> Seq.sumBy (fun (s, b) -> if b then 0 else int s)

let checkWin (board : Board) : bool =
    let getColumn (A:_[,]) c = A.[*,c]
    let getRow (A:_[,]) r = A.[r,*]
    let rows = Array.init 5 (getRow board)
    let columns = Array.init 5 (getColumn board)
    Array.append rows columns
    |> Array.exists (Array.forall (snd))

let printAnswer i (finalBoard : Board array, _ : bool, lastNumber : string) = 
    finalBoard |> Array.exactlyOne |> sumOfUnmarkeds |> (*) (int lastNumber)
    |> printfn "Part %i: %i" i

let solver f (boards : Board array, solved : bool, lastNumber : string) (number : string) =
    if solved then boards, true, lastNumber
    else f (boards, solved, lastNumber) number 

let part1Solver = 
    solver (fun (boards, _, _) number ->
        let newBoards = boards |> Array.map (markNumber number)
        match newBoards |> Array.tryPick (fun board -> if checkWin board then Some board else None) with
        | Some winningBoard -> [|winningBoard|], true, number
        | None -> newBoards, false, number
    )

let part2Solver = 
    solver (fun (boards, _, _) number ->
        let newBoards = boards |> Array.map (markNumber number)
        match newBoards |> Array.filter (checkWin >> not) with
        | [||] -> newBoards, true, number
        | filtered -> filtered, false, number
    )

numbers
|> List.fold part1Solver (bingoBoards, false, "")
|> printAnswer 1

numbers
|> List.fold part2Solver (bingoBoards, false, "")
|> printAnswer 2
