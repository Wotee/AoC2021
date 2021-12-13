let (|Forward|Up|Down|) (x : string) = 
    match x.Split(' ') with
    | [|"forward"; y|] -> Forward (int y)
    | [|"up"; y|] -> Up (int y)
    | [|"down"; y|] -> Down (int y)
    | _ -> failwithf "Unexpected input %A" x

let input =
    System.IO.File.ReadAllLines("./inputs/day2.txt")

#time

input
|> Array.fold (fun (x, y) -> function Forward n -> x+n, y | Up n -> x, y-n | Down n -> x, y+n) (0,0)
|> fun (a,b) -> a*b
|> printfn "Part1: %i"

input
|> Array.fold (fun (x,y,aim) -> function Forward n -> x+n, y+(aim*n), aim | Up n -> x, y, aim-n | Down n -> x, y, aim+n) (0,0,0)
|> fun (x,y,_) -> x*y
|> printfn "Part2: %i"