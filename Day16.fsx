let input = System.IO.File.ReadAllText("inputs/day16.txt")

let hexToBinary (c : char) = System.Convert.ToString(System.Convert.ToInt32(string c, 16), 2).PadLeft(4, '0')
let convertHexToBinary : string -> string = (Seq.collect hexToBinary) >> System.String.Concat

type PacketData =
    | LiteralValue of int64
    | Operation of Packet seq
and Packet = 
    {  Version : int
       TypeId : int
       PacketData : PacketData  }

module Packet = 
    let rec sumVersions (packet : Packet) = 
        match packet.PacketData with
        | LiteralValue _ -> packet.Version
        | Operation packets -> packet.Version + Seq.sumBy sumVersions packets 

    let rec evaluate (packet : Packet)  : int64 = 
        let evaluateOperation (packets : Packet seq) f =
            Seq.map evaluate packets |> f
        let evaluateComparisonOperation o f =
            evaluateOperation o (Seq.toArray >> fun a -> if f a[0] a[1] then 1 else 0)

        match packet.TypeId, packet.PacketData with
        | 0, Operation o -> evaluateOperation o (Seq.reduce (+))
        | 1, Operation o -> evaluateOperation o (Seq.reduce (*))
        | 2, Operation o -> evaluateOperation o (Seq.min)
        | 3, Operation o -> evaluateOperation o (Seq.max)
        | 4, LiteralValue x -> x
        | 5, Operation o -> evaluateComparisonOperation o (>)
        | 6, Operation o -> evaluateComparisonOperation o (<)
        | 7, Operation o -> evaluateComparisonOperation o (=)
        | _ -> failwith "Invalid combination"

let rec (|LiteralPacket|OperationPacket|) (x : string) = 
    match System.Convert.ToInt32(x[3..5], 2) with
    | 4 ->
        let last = 
            x[6..]
            |> Seq.chunkBySize 5
            |> Seq.takeWhile (fun chunk -> chunk[0] = '1')
            |> Seq.length
            |> fun count -> 6 + ((count + 1) * 5) - 1
        LiteralPacket (x[..last], x[last + 1..])
    | _ ->
        let last = 
            match x[6] with
            | '0' -> System.Convert.ToInt32(x[7..21], 2) + 21
            | _ ->
                let len = System.Convert.ToInt32(x[7..17], 2)
                17 + (x[18..] |> getPacketStrings |> Seq.truncate len |> Seq.concat |> Seq.length)
        OperationPacket (x[..last], x[last + 1..])
and getPacketStrings (input : string) = 
    let rec split (x : string) = 
        seq {
            match x with
            | LiteralPacket (packet, rest)
            | OperationPacket (packet, rest) ->
                yield packet
                if rest.Length >= 11 then
                    yield! split rest
        }
    split input 

let rec parse (str: string) : Packet seq = 
    let parseLiteralValue (str: string) : PacketData =
        str[6..]
        |> Seq.chunkBySize 5
        |> Seq.filter (fun chunk -> chunk.Length = 5)
        |> Seq.collect Seq.tail
        |> System.String.Concat
        |> fun str -> System.Convert.ToInt64(str, 2)
        |> LiteralValue
    let parseOperation (str: string) : PacketData =
        let lengthTypeID = System.Convert.ToInt32(str.[6] |> string)
        match lengthTypeID with
        | 0 -> str[22..] |> parse
        | _ -> str[18..] |> parse
        |> Operation
    str
    |> getPacketStrings
    |> Seq.map (fun packetString -> 
        let typeId = System.Convert.ToInt32(packetString.[3..5], 2)
        let data = match typeId with | 4 -> packetString |> parseLiteralValue | _ -> packetString |> parseOperation
        {  Version = System.Convert.ToInt32(packetString[..2], 2)
           TypeId = typeId
           PacketData = data }
    )

let packet = 
    input
    |> convertHexToBinary
    |> parse
    |> Seq.exactlyOne

Packet.sumVersions packet |> printfn "Part 1: %i"
Packet.evaluate packet |> printfn "Part 2: %i"
