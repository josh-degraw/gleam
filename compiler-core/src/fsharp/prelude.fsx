namespace gleam

// Re-export some core types under the gleam namespace
type Dict<'key, 'value when 'key: comparison> = Map<'key, 'value>
type Set<'key when 'key: comparison> = Microsoft.FSharp.Collections.Set<'key>
type Option<'a> = Microsoft.FSharp.Core.Option<'a>
type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>
type StringBuilder = System.Text.StringBuilder
type Regex = System.Text.RegularExpressions.Regex

// Manual Override
type Iterator<'a> = System.Collections.Generic.IEnumerable<'a>


// Gleam-specific types


type BitArray(buffer: byte[]) =
    member val internal _buffer = buffer with get, set
    member this.ByteAt(index: int64) = this._buffer.[int index]

    member this.FloatFromSlice(start: int64, end': int64) =
        let start = int start
        let end' = int end'
        let byteSize = end' - start

        let slice = this._buffer.[start..end']

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

        let slice = this._buffer.[start..end']

        if byteSize = 8 then
            System.BitConverter.ToInt64(slice, 0)
        else if byteSize = 4 then
            System.BitConverter.ToInt32(slice, 0)
        else
            failwith $"Sized integers must be 32-bit or 64-bit on .NET, got size of {byteSize * 8} bits"

    member this.BinaryFromSlice(start: int64, end': int64) =
        let start = int start
        let end' = int end'
        let slice = this._buffer.[start..end']
        BitArray(slice)

    member this.SliceAfter(index: int64) =
        let index = int index
        let slice = this._buffer.[index..]
        BitArray(slice)


    static member Create(segments: BitArraySegment list) =
        let size segment =
            match segment.value with
            | Bits ba -> ba._buffer.Length
            | Bytes bytes -> bytes.Length
            | Float _
            | Int _
            | Utf8 _
            | Utf16 _
            | Utf32 _
            | Utf8Codepoint _
            | Utf16Codepoint _
            | Utf32Codepoint _ -> 8

        let byte_length = segments |> List.map size |> List.sum

        let buffer = Array.zeroCreate byte_length

        let cursor = 0

        for segment in segments do

            let valueBuffer = segment.ToBytes()

            System.Buffer.BlockCopy(valueBuffer, 0, buffer, 0, valueBuffer.Length)

        BitArray(buffer)


and BitArraySegmentValue =
    | Bits of BitArray
    | Bytes of byte[]
    | Float of float
    | Int of int64
    | Utf8 of string
    | Utf16 of string
    | Utf32 of string
    | Utf8Codepoint of int64
    | Utf16Codepoint of int64
    | Utf32Codepoint of int64

and BitArrayEndianness =
    | Big
    | Little
    | Native

and BitArraySegmentSign =
    | Signed
    | Unsigned


and BitArraySegment =
    { endianness: BitArrayEndianness option
      size: int64 option
      unit: int64 option
      signed: BitArraySegmentSign option
      value: BitArraySegmentValue }

    member this.ToBytes() =
        match this.value with
        | Bits ba -> ba._buffer
        | Bytes bytes -> bytes
        | Float f -> System.BitConverter.GetBytes(f)
        | Int i when this.signed = Some Unsigned -> System.BitConverter.GetBytes(uint64 i)
        | Int i -> System.BitConverter.GetBytes(i)
        | Utf8 s -> System.Text.Encoding.UTF8.GetBytes(s)
        | Utf16 s when this.endianness = Some Big -> System.Text.Encoding.BigEndianUnicode.GetBytes(s)
        | Utf16 s -> System.Text.Encoding.Unicode.GetBytes(s)
        | Utf32 s -> System.BitConverter.GetBytes(uint32 s)
        | Utf8Codepoint i -> System.Text.Encoding.UTF8.GetBytes(string (char i))
        | Utf16Codepoint i when this.endianness = Some Big ->
            System.Text.Encoding.BigEndianUnicode.GetBytes(string (char i))
        | Utf16Codepoint i -> System.Text.Encoding.Unicode.GetBytes(string (char i))
        | Utf32Codepoint i -> System.Text.Encoding.UTF32.GetBytes(string (char i))


[<Struct>]
type UtfCodepoint = UtfCodepoint of int64

[<CustomEquality; CustomComparison>]
type Dynamic =
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

type DecodeError =
    { expected: string
      found: string
      path: list<string> }

type DecodeErrors = list<DecodeError>
type UnknownTuple = UnknownTuple of Dynamic list

type Order =
    | Lt
    | Eq
    | Gt

type Match =
    { content: string
      submatches: list<Option<string>> }

type RegexOptions =
    { case_insensitive: bool
      multi_line: bool }

type CompileError = { error: string; byte_index: int64 }

type Uri =
    { scheme: Option<string>
      userinfo: Option<string>
      host: Option<string>
      port: Option<int64>
      path: string
      query: Option<string>
      fragment: Option<string> }

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
