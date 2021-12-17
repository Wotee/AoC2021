// let input = System.IO.File.ReadAllLines("inputs/day16.txt")
let input = System.IO.File.ReadAllLines("inputs/day16-sample.txt")

let hexToBinary (c : char) = System.Convert.ToString(System.Convert.ToInt32(string c, 16), 2).PadLeft(4, '0')

let convertHexToBinary : string -> string = (Seq.collect hexToBinary) >> System.String.Concat    

let test = "D2FE28" |> convertHexToBinary
let test2 = "38006F45291200" |> convertHexToBinary

let (|Literal|Operator|) (x : string) =
    match (System.Convert.ToInt32(x, 2)) with
    | 4 -> Literal
    | _ -> Operator

let parseLiteral (input : string) = 
    let rec parseWindow (asd : string) =
        seq {
            let data, rest = asd[..4], asd[5..]
            yield data[1..]
            if data[0] = '1' then yield! parseWindow rest
            else yield rest
        }
    let data = parseWindow input |> Seq.toArray
    let content, rest = data[..^1], data |> Array.last
    let packetLength = content.Length + 6
    let extraCharacters = 4 - (packetLength % 4)
    content |> System.String.Concat, rest[extraCharacters..]

// TODO: Implement this
let parseOperator (input : string) =
    match input[0] with
    | '0' ->
        let l, rest = input[1..15],  input[16..]
        l, rest, ""
    | _ ->
        input, input, ""

let parsePackets (input : string) = 
    let rec parsePacket (input : string) = 
        seq {
            let packetVersion, packetType, rest = input[..2], input[3..5], input[6..]
            match packetType with
            | Literal ->
                let literalContent, nextPackets = parseLiteral rest
                yield (packetVersion, packetType, literalContent)
                if System.String.IsNullOrWhiteSpace nextPackets |> not then
                    yield! parsePacket nextPackets
            | Operator ->
                yield parseOperator rest
        }
    parsePacket input

parsePackets test
parsePackets test2