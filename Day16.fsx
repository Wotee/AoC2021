// let input = System.IO.File.ReadAllLines("inputs/day16.txt")
// let input = System.IO.File.ReadAllLines("inputs/day16-sample.txt")

let hexToBinary (c : char) = System.Convert.ToString(System.Convert.ToInt32(string c, 16), 2).PadLeft(4, '0')
let convertHexToBinary : string -> string = (Seq.collect hexToBinary) >> System.String.Concat

type PacketVersion = 
    Version of int
type DummyPacket = 
    {  version : PacketVersion
       literal : int option  }

let (|Literal|Operator|) (x : string) =
    match (System.Convert.ToInt32(x, 2)) with
    | 4 -> Literal
    | _ -> Operator

let (|DataGroup|) (x : string) = 
    DataGroup x[1..4]

let (|LastDataGroup|_|) (x : string) = 
    if x[0] = '0' then Some LastDataGroup else None

let (|NonDataGroup|_|) (x : string) =
    Some (NonDataGroup x.[5..])

let (|Content|_|) (x : 'a seq) = 
    let a = x |> Seq.toArray |> fun x -> x[..^1]
    Some (Content a |> System.String.Concat)

let (|NonContent|_|) (x : string seq) = 
    let a = x |> Seq.toArray
    // let content = a[..^1]
    // let packetLength = content.Length + 6
    // let extraCharacters = 4 - (packetLength % 4)
    // printfn "There are maybe %i extra charactrers" extraCharacters
    // Some (NonContent a |> Array.last |> fun x -> x[extraCharacters..])
    // TODO: To Seq?
    Some (NonContent a |> Array.last)

let (|PacketVersion|_|) (x : string) =
    Some (PacketVersion (System.Convert.ToInt32(x[..2], 2)))

let (|PacketType|_|) (x : string) = 
    Some (PacketType x.[3..5])

let (|DataAndTail|_|) (x : string) =
    Some (DataAndTail x.[6..])

let parseLiteral (input : string) = 
    let rec parseDataGroup data = 
        seq {
            match data with
            | LastDataGroup & DataGroup dataGroup & NonDataGroup rest ->
                yield dataGroup
                yield rest
            | DataGroup dataGroup & NonDataGroup rest ->
                yield dataGroup
                yield! parseDataGroup rest
            | _ -> failwith "Unexpected data"
        }
    match parseDataGroup input with
    | Content content & NonContent rest -> content, rest
    | _ -> failwith "Failed to parse input"

let (|LengthInBits|NumberOfSubPackets|) (x : string) =
    if x[0] = '0' then LengthInBits else NumberOfSubPackets

let (|TotalLength|) (x : string) = 
    TotalLength (System.Convert.ToInt32(x[1..15], 2))

let (|LenTail|) (x : string) = 
    LenTail x[16..]

let (|SubPacketNumber|) (x : string) = 
    SubPacketNumber (System.Convert.ToInt32(x[1..11], 2))

let (|SubTail|) (x : string) = 
    SubTail x[12..]

let rec parseOperator (input : string) =
    match input with
    | LengthInBits & TotalLength len & LenTail tail ->
        printfn "Lengt in bits part found"
        // TODO: The rest might not be needed. Maybe always zeroes?
        let subPackets, rest = tail[..len], tail[len+1..]
        assert (System.Convert.ToInt32(rest, 2) = 0)
        parsePackets subPackets
    | NumberOfSubPackets & SubPacketNumber nmbr & SubTail tail ->
        printfn "Number of subpackets found %i" nmbr
        // TODO: This probably has a bug in continuation
        printfn "There is the tail: %A" tail
        parsePackets tail
and parsePackets (input : string) : DummyPacket seq = 
    let rec parsePacket (input : string) = 
        seq {
            match input with
            | PacketVersion v & PacketType Literal & DataAndTail rest -> 
                printfn "Literal packet found"
                let literalContent, nextPackets = parseLiteral rest
                let content = System.Convert.ToInt32(literalContent, 2)
                yield { version = Version v; literal = Some content}
                if System.Convert.ToInt32(nextPackets, 2) <> 0 then
                    yield! parsePacket nextPackets
            | PacketVersion v & PacketType Operator & DataAndTail rest ->
                printfn "Operator packet found"
                yield { version = Version v; literal = None }
                yield! parseOperator rest
            | _ -> failwith "Unknown input data format"
        }
    parsePacket input

let test1 = "8A004A801A8002F478" |> convertHexToBinary
let test2 = "620080001611562C8802118E34" |> convertHexToBinary
let test3 = "C0015000016115A2E0802F182340" |> convertHexToBinary
let test4 = "A0016C880162017C3686B18A3D4780" |> convertHexToBinary

parsePackets test1 |> Seq.sumBy (fun { version = Version v } ->  v) |> fun x -> assert (x = 16)
parsePackets test2 |> Seq.sumBy (fun { version = Version v } ->  v)