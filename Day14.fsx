#load "Common.fsx"
#load "Inputs.fsx"
open Common
open System

let maskRegex = "mask = (.*)"
let updateRegex = "mem\[(\d*)\] = (\d*)"
let parse input = input |> lines

let readMask =
    function
    | 'X' -> None
    | '0' -> Some 0L
    | '1' -> Some 1L

let rec intToBinary i =
    match i with
    | 0L
    | 1L -> string i
    | _ ->
        let bit = string (i % 2L)
        (intToBinary (i / 2L)) + bit

let binaryToInt a =
    a
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy ((fun (i, value) -> (2.0 ** (i |> float) |> int64) * value))

let maskedNumber mask value =
    let paddedValue =
        value
        |> intToBinary
        |> fun s -> s.PadLeft(36, '0')
        |> Seq.map (fun c -> (int64 c) - (int64 '0'))

    let sequence =
        Seq.map2 (fun masked v -> Option.defaultValue v masked) mask paddedValue
    // |> Seq.map (fun c -> c - (int '0'))

    sequence |> Seq.toArray

let getMask instruction =
    extractValues maskRegex instruction
    |> Option.get
    |> Seq.head
    |> Seq.toArray
    |> Array.map readMask

let fluctuatingAddresses mask address =
    let rec inner prefix maskBody addressBody =
        match List.tryHead maskBody, List.tryHead addressBody with
        | None, _ -> [ prefix ]
        | Some None, _ ->
            List.append
                (inner (prefix + "0") (List.tail maskBody) (List.tail addressBody))
                (inner (prefix + "1") (List.tail maskBody) (List.tail addressBody))
        | Some (Some 0L), Some c -> inner (prefix + c) (List.tail maskBody) (List.tail addressBody)
        | Some (Some 1L), _ -> inner (prefix + "1") (List.tail maskBody) (List.tail addressBody)

    inner "" mask address

let part1 input =
    let lines = input |> parse

    let inner (mask: option<int64> []) (instruction: string) memory =
        match instruction.[1] with
        | 'a' -> // update mask
            let (newMask: option<int64> []) = getMask instruction

            newMask, memory
        | 'e' -> // assign mem
            let [ loc; value ] =
                extractValues updateRegex instruction
                |> Option.get
                |> Seq.toList
                |> List.map int64

            let updateMemory =
                Map.add loc (maskedNumber mask value) memory


            mask, updateMemory

    Array.fold (fun (mask, memory) line -> inner mask line memory) (Array.create 36 None, Map.empty) lines
    |> snd

let part2 input =
    let lines = input |> parse

    let inner mask (instruction: string) memory =
        match instruction.[1] with
        | 'a' -> getMask instruction, memory
        | 'e' ->
            let [ loc; value ] =
                extractValues updateRegex instruction
                |> Option.get
                |> Seq.toList
                |> List.map int64

            let addresses =
                fluctuatingAddresses
                    (mask |> Array.toList)
                    (intToBinary loc
                     |> fun s -> s.PadLeft(36, '0') |> Seq.map string |> Seq.toList)
                |> Seq.map
                    ((Seq.map (fun c -> int64 c - int64 '0'))
                     >> binaryToInt)

            mask,
            addresses
            |> Seq.fold (fun acc address -> Map.add address value acc) memory

    Array.fold (fun (mask, memory) line -> inner mask line memory) (Array.create 36 None, Map.empty) lines
    |> snd

let sumValues m = m |> values |> Seq.sumBy binaryToInt

let input = Inputs.day14

let sample = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"

let sample2 = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

input |> part2 |> values |> Seq.sum
