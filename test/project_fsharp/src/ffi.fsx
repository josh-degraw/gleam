module rec my.``mod``

#load "../../../compiler-core/src/fsharp/prelude.fs"
#load "../../../external/stdlib/src/gleam_stdlib.fs"

open gleam

let private go (x: UtfCodepoint) =
    begin
        BitArray.Create(
            {
                endianness = None
                size = None
                unit = None
                signed = None
                value = BitArraySegmentValue.Utf8Codepoint(x)
            },
            {
                endianness = None
                size = None
                unit = None
                signed = None
                value = BitArraySegmentValue.Utf8(System.Text.Encoding.UTF8.GetBytes("Gleam"))
            }
        )
    end
