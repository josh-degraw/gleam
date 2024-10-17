namespace gleam

open System

// Re-export some core types under the gleam namespace
type Dict<'key, 'value when 'key: comparison> = Map<'key, 'value>
type Set<'key when 'key: comparison> = Microsoft.FSharp.Collections.Set<'key>
type Option<'a> = Microsoft.FSharp.Core.Option<'a>
type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>
type StringBuilder = System.Text.StringBuilder
type Regex = System.Text.RegularExpressions.Regex

// Manual Override
//type Iterator<'a> = System.Collections.Generic.IEnumerable<'a>

// Gleam-specific types

// For tuples above 1 element we can use built-in tuples
// But gleam supports tuples of any size so we need to define our own
// to represent an empty tuple and a single item tuple
// type Tuple1<'a> = Tuple1 of 'a

type EmptyTuple = EmptyTuple

[<Struct>]
type UtfCodepoint = UtfCodepoint of System.Text.Rune

// type BitArrayOption =
//     | Bytes
//     | Int
//     | Float
//     | Bits
//     | Utf8
//     | Utf16
//     | Utf32
//     | Utf8Codepoint
//     | Utf16Codepoint
//     | Utf32Codepoint
//     | Signed
//     | Unsigned
//     | Big
//     | Little
//     | Native
//     | Size of int64
//     | Unit of int64

[<Struct>]
type BitArray(buffer: byte[]) =

    // new() = BitArray(Array.empty)
    // new(size: int) = BitArray(Array.zeroCreate size)
    //member val internal _buffer = buffer with get/, set

    member internal this.Buffer = buffer

    static member Empty = BitArray()

    member this.ByteAt(index: int64) = buffer.[int index]

    member this.FloatFromSlice(start: int64, end': int64) =
        let start = int start
        let end' = int end'
        let byteSize = end' - start

        let slice = buffer[start..end']

        if byteSize = 8 then
            System.BitConverter.ToDouble(slice, 0)
        else if byteSize = 4 then
            System.BitConverter.ToSingle(slice, 0) |> float
        else
            failwith $"Sized floats must be 32-bit on .NET, got size of {byteSize * 8} bits"

    member this.IntFromSlice(start: int64, end': int64) : int64 =
        let start = int start
        let end' = int end'
        let byteSize = end' - start

        let slice = buffer.[start..end']

        if byteSize = 8 then
            System.BitConverter.ToInt64(slice, 0)
        else if byteSize = 4 then
            System.BitConverter.ToInt32(slice, 0)
        else
            failwith $"Sized integers must be 32-bit or 64-bit on .NET, got size of {byteSize * 8} bits"

    member this.BinaryFromSlice(start: int64, end': int64) =
        let start = int start
        let end' = int end'
        let slice = buffer[start..end']
        BitArray(slice)

    member this.SliceAfter(index: int64) =
        let index = int index
        let slice = buffer[index..]
        BitArray(slice)


    static member Create([<ParamArray>] segments: BitArraySegment[]) =
        let size segment =
            match segment.size with
            | Some size -> int size
            | None ->
                match segment.value with
                | Bits ba -> ba.Buffer.Length
                | Bytes bytes -> bytes.Length
                | Float _
                | Int _
                | Utf8 _
                | Utf16 _
                | Utf32 _
                | Utf8Codepoint _
                | Utf16Codepoint _
                | Utf32Codepoint _ -> 8

        let byte_length = segments |> Array.map size |> Array.sum

        let mutable buffer = Array.zeroCreate byte_length

        for segment in segments do

            let valueBuffer = segment.ToBytes()

            System.Buffer.BlockCopy(valueBuffer, 0, buffer, 0, valueBuffer.Length)

        BitArray(buffer)


and [<Struct>] BitArraySegmentValue =
    | Bits of bits: BitArray
    | Bytes of bytes: byte[]
    | Float of float: float
    | Int of int: int64
    | Utf8 of utf8: byte[]
    | Utf16 of utf16: byte[]
    | Utf32 of utf32: byte[]
    | Utf8Codepoint of utf8Codepoint: UtfCodepoint
    | Utf16Codepoint of utf16Codepoint: byte[]
    | Utf32Codepoint of utf32Codepoint: byte[]

and [<Struct>] BitArrayEndianness =
    | Big
    | Little
    | Native

and [<Struct>] BitArraySegment = {
    endianness: BitArrayEndianness option
    size: int64 option
    unit: int64 option
    signed: bool option
    value: BitArraySegmentValue
} with

    static member Bytes(bytes: byte[]) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Bytes bytes
    }

    static member Int(int: int64, ?endianness: BitArrayEndianness, ?signed: bool) = {
        endianness = endianness
        size = None
        unit = None
        signed = signed
        value = Int int
    }

    static member Float(float: float, ?endianness: BitArrayEndianness) = {
        endianness = endianness
        size = None
        unit = None
        signed = None
        value = Float float
    }

    static member Bits(bits: BitArray) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Bits bits
    }

    static member Utf8(utf8: string) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf8(System.Text.Encoding.UTF8.GetBytes(utf8))
    }

    static member Utf16(utf16: string) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf16(System.Text.Encoding.Unicode.GetBytes(utf16))
    }

    static member Utf32(utf32: string) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf32(System.Text.Encoding.UTF32.GetBytes(utf32))
    }

    static member Utf8Codepoint(utf8Codepoint: UtfCodepoint) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf8Codepoint utf8Codepoint
    }

    static member Utf16Codepoint(utf16Codepoint: byte[]) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf16Codepoint utf16Codepoint
    }

    static member Utf32Codepoint(utf32Codepoint: byte[]) = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Utf32Codepoint utf32Codepoint
    }

    // static member Unit(unit: int64) = {
    //     endianness = None
    //     size = None
    //     unit = Some unit
    //     signed = None
    //     value = Unit unit
    // }

    // static member Size(size: int64) = {
    //     endianness = None
    //     unit = None
    //     signed = None
    //     value = Size size
    // }

    member this.ToBytes() =
        match this.value with
        | Bits ba -> ba.Buffer
        | Bytes bytes -> bytes
        | Float f -> System.BitConverter.GetBytes(f)
        | Int i when this.signed = Some true -> System.BitConverter.GetBytes(uint64 i)
        | Int i -> System.BitConverter.GetBytes(i)
        | Utf8Codepoint(UtfCodepoint cp) -> System.Text.Encoding.UTF8.GetBytes(string cp)
        | Utf8 bytes
        | Utf16 bytes
        | Utf32 bytes
        | Utf16Codepoint bytes
        | Utf16Codepoint bytes
        | Utf32Codepoint bytes -> bytes

[<CustomEquality; CustomComparison>]
type Dynamic =
    private
    | Dynamic of obj

    interface System.IComparable with
        member this.CompareTo(obj) =
            System.StringComparer.InvariantCulture.Compare($"%A{this}", $"%A{obj}")

    override this.Equals(obj) =
        match obj with
        | :? Dynamic as dynamic -> System.StringComparer.InvariantCulture.Equals($"%A{this}", $"%A{dynamic}")
        | _ -> false

    override this.GetHashCode() =
        System.StringComparer.InvariantCulture.GetHashCode($"%A{this}")

    static member From(a: obj) : Dynamic =
        match a with
        | :? Dynamic as (Dynamic(d)) -> Dynamic.From d
        | a -> Dynamic(a)

type DecodeError = {
    expected: string
    found: string
    path: list<string>
}

type DecodeErrors = list<DecodeError>
type UnknownTuple = UnknownTuple of Dynamic list

type Order =
    | Lt
    | Eq
    | Gt

type Match = {
    content: string
    submatches: list<Option<string>>
}

type RegexOptions = {
    case_insensitive: bool
    multi_line: bool
}

type CompileError = { error: string; byte_index: int64 }

type Uri = {
    scheme: Option<string>
    userinfo: Option<string>
    host: Option<string>
    port: Option<int64>
    path: string
    query: Option<string>
    fragment: Option<string>
}

[<AutoOpen>]
module Prelude =
    /// Check if a string starts with a prefix
    let (|Gleam__codegen__prefix|_|) (prefix: string) (target: string) : string option =
        if target.StartsWith(prefix) then
            Some(target.Substring(prefix.Length))
        else
            None

    /// Split a string into a prefix and a suffix
    let (|Gleam_codegen_string_parts|_|) (p: string) (s: string) : Option<string * string> =
        if s.StartsWith(p) then
            Some(p, s.Substring(p.Length))
        else
            None

    let (|Dynamic|) (Dynamic v) = v

    let (|Tuple1|) (t: System.Tuple<'a>) = t.Item1
    let Tuple1 (a: 'a) = System.Tuple.Create(a)
