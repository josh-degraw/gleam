namespace gleam

/// WIP
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
            match segment.size with
            | Some size -> int size
            | None ->
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

        let mutable buffer = Array.zeroCreate byte_length

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
