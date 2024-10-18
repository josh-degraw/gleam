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

[<Struct; CustomEquality; CustomComparison>]
type BitArray private (_buffer: byte[]) =
    static let b64EncodeLookup = [
        65uy
        66uy
        67uy
        68uy
        69uy
        70uy
        71uy
        72uy
        73uy
        74uy
        75uy
        76uy
        77uy
        78uy
        79uy
        80uy
        81uy
        82uy
        83uy
        84uy
        85uy
        86uy
        87uy
        88uy
        89uy
        90uy
        97uy
        98uy
        99uy
        100uy
        101uy
        102uy
        103uy
        104uy
        105uy
        106uy
        107uy
        108uy
        109uy
        110uy
        111uy
        112uy
        113uy
        114uy
        115uy
        116uy
        117uy
        118uy
        119uy
        120uy
        121uy
        122uy
        48uy
        49uy
        50uy
        51uy
        52uy
        53uy
        54uy
        55uy
        56uy
        57uy
        43uy
        47uy
    ]

    member internal this.Buffer = if isNull _buffer then Array.empty else _buffer

    static member Empty = BitArray()

    override this.Equals(obj) =
        match obj with
        | :? BitArray as other when other.Length = this.Length ->
            System.Linq.Enumerable.SequenceEqual(this.Buffer, other.Buffer)
        | _ -> false

    override this.GetHashCode() = System.HashCode.Combine(this.Buffer)

    member this.ByteAt(index: int64) = this.Buffer.[int index]

    member this.FloatFromSlice(start: int64, end': int64) =
        let start = int start
        let end' = int end'
        let byteSize = end' - start

        let slice = this.Buffer[start..end']

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

        let slice = this.Buffer[start..end']

        if byteSize = 8 then
            System.BitConverter.ToInt64(slice, 0)
        else if byteSize = 4 then
            System.BitConverter.ToInt32(slice, 0)
        else
            failwith $"Sized integers must be 32-bit or 64-bit on .NET, got size of {byteSize * 8} bits"


    member this.Slice(start: int64, length: int64) : Result<BitArray, unit> =
        let start = int start
        let end' = int length + start - 1

        if start = 0 && length = 0 then
            Ok(BitArray.Empty)
        elif length = 0 then
            Error()
        elif length < 0 && start = this.Buffer.Length then
            let start = this.Buffer.Length + int length
            let slice = this.Buffer[start..]
            Ok(BitArray(slice))
        elif start < 0 || end' < 0 || end' > this.Buffer.Length then
            Error()
        else
            let slice = this.Buffer[start..end']
            Ok(BitArray(slice))

    member this.SliceAfter(index: int64) =
        let index = int index
        let slice = this.Buffer[index..]
        BitArray(slice)

    member this.Length = this.Buffer.Length

    member this.TryToUtf8String() =
        try
            if this.IsUtf8() then
                Ok(System.Text.Encoding.UTF8.GetString(this.Buffer))
            else
                Error()
        with _ ->
            Error()

    member this.IsUtf8() =
        let rec isValidUtf8 (bytes: byte[]) (index: int) =
            if index >= bytes.Length then
                true
            else
                let byte = bytes.[index]

                if byte <= 127uy then
                    // ASCII character
                    isValidUtf8 bytes (index + 1)
                elif byte >= 192uy && byte <= 223uy then
                    // 2-byte sequence
                    if index + 1 >= bytes.Length || (bytes.[index + 1] &&& 192uy) <> 128uy then
                        false
                    else
                        isValidUtf8 bytes (index + 2)
                elif byte >= 224uy && byte <= 239uy then
                    // 3-byte sequence
                    if
                        index + 2 >= bytes.Length
                        || (bytes.[index + 1] &&& 192uy) <> 128uy
                        || (bytes.[index + 2] &&& 192uy) <> 128uy
                    then
                        false
                    else
                        isValidUtf8 bytes (index + 3)
                elif byte >= 240uy && byte <= 247uy then
                    // 4-byte sequence
                    if
                        index + 3 >= bytes.Length
                        || (bytes.[index + 1] &&& 192uy) <> 128uy
                        || (bytes.[index + 2] &&& 192uy) <> 128uy
                        || (bytes.[index + 3] &&& 192uy) <> 128uy
                    then
                        false
                    else
                        isValidUtf8 bytes (index + 4)
                else
                    false

        isValidUtf8 this.Buffer 0


    // Copied from js implementation to account for padding
    member this.Base64Encode(padding: bool) =
        let base64 = Convert.ToBase64String(this.Buffer)

        if padding then
            base64
        else
            base64.Replace("==", "").Replace("=", "")

    static member Base64Decode(encoded: string) : Result<BitArray, unit> =
        if encoded.Length = 0 then
            Ok(BitArray.Empty)
        else
            try
                let bytes = Convert.FromBase64String(encoded)
                BitArray.FromBytes(bytes) |> Ok
            with _ ->
                Error()

    member this.Base16Encode() : string =
        let result = StringBuilder()

        let s = System.Text.Encoding.UTF8.GetString(this.Buffer)

        for byte in this.Buffer do
            // Trim leading zeros if this was encoded as integers
            // TODO: Could this be optimized to save space on storage?
            result.Append(byte.ToString("x2").ToUpper().Trim('0')) |> ignore

        result |> string

    static member Base16Decode(hex: string) : Result<BitArray, unit> =
        try
            Convert.FromHexString(hex) |> BitArray.FromBytes |> Ok
        with e ->
            Error()

    member this.Compare(obj: BitArray) : Order =
        let comp = compare this.Buffer obj.Buffer

        if comp = 0 then Eq
        elif comp < 0 then Lt
        else Gt

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? BitArray as other -> compare this.Buffer other.Buffer
            | _ -> invalidArg "obj" "Cannot compare BitArray with non-BitArray object"

    member this.MatchSegments([<ParamArray>] matchSegments: BitArraySegment[]) =
        let bytes = this.Buffer

        let mutable cursor = 0
        let mutable hasFailure = false

        let res = [|

            for segment in matchSegments do
                if not hasFailure then
                    let valueBuffer: byte[] = segment.ToBytes()

                    if valueBuffer.Length > bytes.Length then
                        hasFailure <- true

                    else
                        let slice = bytes[cursor .. (cursor + valueBuffer.Length - 1)]

                        if slice = valueBuffer then
                            cursor <- cursor + valueBuffer.Length
                            segment.value
                        else
                            hasFailure <- true
            if cursor < bytes.Length then
                Bytes bytes[cursor..]
        |]

        if hasFailure then None else Some res

    // Factory methods

    static member FromBytes(bytes: byte[]) = BitArray(bytes)

    static member FromString(str: string) =
        BitArray(System.Text.Encoding.UTF8.GetBytes(str))

    static member Concat(bit_arrays: BitArray seq) =
        let buffer = Array.concat [ for ba in bit_arrays -> ba.Buffer ]
        BitArray(buffer)

    static member Create([<ParamArray>] segments: BitArraySegment[]) =
        let size segment =
            match segment.size with
            | Some size -> int size
            | None -> segment.Length

        let byte_length = segments |> Array.map size |> Array.sum

        let mutable buffer = Array.zeroCreate byte_length
        let mutable cursor = 0

        for segment in segments do

            let valueBuffer = segment.ToBytes()

            System.Buffer.BlockCopy(valueBuffer, 0, buffer, cursor, valueBuffer.Length)
            cursor <- cursor + valueBuffer.Length

        BitArray(buffer)

    override this.ToString() =
        let builder = System.Text.StringBuilder()
        builder.Append("<<") |> ignore
        let mutable i = 0

        for b in this.Buffer do
            builder.Append(sprintf "%i" b) |> ignore

            if i + 1 < this.Buffer.Length then
                builder.Append(", ") |> ignore

            i <- i + 1

        builder.Append(">>") |> string


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

    static member FromString(str: string) =
        BitArraySegmentValue.Utf16(System.Text.Encoding.Unicode.GetBytes(str))

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

    static member Empty = {
        endianness = None
        size = None
        unit = None
        signed = None
        value = Bytes Array.empty
    }

    member this.Length =
        match this.value with
        | Bytes bytes
        | Utf8 bytes
        | Utf16 bytes
        | Utf32 bytes -> bytes.Length
        | Utf8Codepoint(UtfCodepoint c) -> c.Utf8SequenceLength
        | Utf16Codepoint _ -> 16
        | Utf32Codepoint _ -> 32
        | Int i when i <= int64 Byte.MaxValue -> sizeof<byte>
        | Int _ when this.signed = Some true -> sizeof<int64>
        | Int _ -> sizeof<uint64>
        | Float _ -> sizeof<double>
        | Bits b -> b.Buffer.Length

    static member FromUtf16String(str: string) = {
        BitArraySegment.Empty with
            value = Utf16(System.Text.Encoding.Unicode.GetBytes(str))
    }

    member this.Equals(bytes: byte[]) = this.ToBytes() = bytes

    member this.ToBytes() =
        match this.value with
        | Bits ba -> ba.Buffer
        | Bytes bytes -> bytes
        | Float f -> System.BitConverter.GetBytes(f)
        | Int i when i <= int64 Byte.MaxValue -> [| byte i |]
        | Int i when this.signed = Some true -> System.BitConverter.GetBytes(uint64 i)
        | Int i -> System.BitConverter.GetBytes(i)
        | Utf8Codepoint(UtfCodepoint(cp: Text.Rune)) -> System.Text.Encoding.UTF8.GetBytes(string cp)
        | Utf8 bytes
        | Utf16 bytes
        | Utf32 bytes
        | Utf16Codepoint bytes
        | Utf32Codepoint bytes -> bytes

and [<Struct>] BitArraySegmentMatch = {
    segment: BitArraySegment
    value: BitArraySegmentValue
}

// let str = BitArray.Create(BitArraySegment.FromUtf16String("Hello, world!"))
// let m = BitArraySegment.FromUtf16String("Hello, ")
// let w = BitArraySegment.FromUtf16String("world")
// let matches = str.MatchSegments(m, w)

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


    let (|BitArraySegments|_|) (segments: BitArraySegment[]) (bitArray: BitArray) =
        //
        bitArray.MatchSegments(segments)
