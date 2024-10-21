module rec my.``mod``

#load "../../../compiler-core/src/fsharp/prelude.fs"
#load "../../../external/stdlib/src/gleam_stdlib.fs"

open gleam

let private go (x: BitArray) =
    begin
        let (BitArray.Empty) = x
        BitArray.Empty
    end
