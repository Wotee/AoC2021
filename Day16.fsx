let input = System.IO.File.ReadAllText("inputs/day16.txt")
// let input = System.IO.File.ReadAllText("inputs/day16-sample.txt")

let hexToBinary (c : char) = System.Convert.ToString(System.Convert.ToInt32(string c, 16), 2).PadLeft(4, '0')
let convertHexToBinary : string -> string = (Seq.collect hexToBinary) >> System.String.Concat

type PacketData =
    | LiteralValue of int64
    | Operation of {| Packets: Packet seq |}
and Packet = 
    {  Version : int
       TypeId : int
       PacketData : PacketData  }

let rec getPacketStrings (bStr: string) = 
    let literalValueSubString (bStr: string) = 
        let last = 
            bStr.[6..]
            |> Seq.chunkBySize 5
            |> Seq.takeWhile (fun chunk -> chunk.[0] = '1')
            |> Seq.length
            |> fun count -> 6 + ((count + 1) * 5) - 1
        bStr.[..last], bStr.[last + 1..]
    let operationSubString (bStr: string) = 
        let lengthTypeID = System.Convert.ToInt32(bStr.[6] |> string)
        let length = 
            match lengthTypeID with
            | 0 -> System.Convert.ToInt32(bStr.[7..21], 2)
            | 1 -> System.Convert.ToInt32(bStr.[7..17], 2)
            | _ -> failwith "Invalid Length Type ID."
        let last = 
            match lengthTypeID with
            | 0 -> 21 + length
            | 1 -> 17 + (bStr.[18..] |> getPacketStrings |> Seq.truncate length |> Seq.concat |> Seq.length)
            | _ -> failwith "Invalid Length Type ID."
        bStr.[..last], bStr.[last + 1..]
    let rec split (bStr: string) = 
        seq {
            if bStr.Length < 11 then
                yield None
            else 
                let typeID = System.Convert.ToInt32(bStr.[3..5], 2)
                let (subString, rest) = 
                    match typeID with
                    | 4 ->
                        literalValueSubString bStr
                    | _ -> operationSubString bStr
                yield Some subString
                yield! split rest
        }
    split bStr |> Seq.choose id

let rec parse (bStr: string) : Packet seq = 
    let parseLiteralValue (bStr: string) : PacketData =
        bStr[6..]
        |> Seq.chunkBySize 5
        |> Seq.filter (fun chunk -> chunk.Length = 5)
        |> Seq.map (fun chunk -> chunk.[1..])
        |> Seq.concat
        |> fun chars -> chars |> Seq.toArray |> System.String
        |> fun bstr -> System.Convert.ToInt64(bstr, 2)
        |> LiteralValue
    let parseOperation (bStr: string) : PacketData =
        let lengthTypeID = System.Convert.ToInt32(bStr.[6] |> string)
        match lengthTypeID with
        | 0 -> bStr[22..] |> parse
        | _ -> bStr[18..] |> parse
        |> fun x -> Operation {| Packets = x |}
    bStr
    |> getPacketStrings
    |> Seq.map (fun packetString -> 
        let typeId = System.Convert.ToInt32(packetString.[3..5], 2)
        let data = match typeId with | 4 -> packetString |> parseLiteralValue | _ -> packetString |> parseOperation
        {  Version = System.Convert.ToInt32(packetString[..2], 2)
           TypeId = typeId
           PacketData = data }
    )

let test1 = "8A004A801A8002F478" |> convertHexToBinary
let test2 = "620080001611562C8802118E34" |> convertHexToBinary
let test3 = "C0015000016115A2E0802F182340" |> convertHexToBinary
let test4 = "A0016C880162017C3686B18A3D4780" |> convertHexToBinary

let rec flatten (packet : Packet) = 
    seq {
        yield packet
        match packet.PacketData with
        | Operation op -> for p in op.Packets do yield! flatten p
        | _ -> ()
    }

let solve = parse >> Seq.exactlyOne >> flatten >> Seq.sumBy (fun x -> x.Version)

test1 |> solve |> printfn "16 = %i"
test2 |> solve |> printfn "12 = %i"
test3 |> solve |> printfn "23 = %i"
test4 |> solve |> printfn "31 = %i"

input |> convertHexToBinary |> solve |> printfn "Part 1: %i" // 895