namespace gleam


[<AutoOpen>]
module Prelude =

    [<AutoOpen>]
    module Builtins =

        // Aliases to .NET builtins
        type Dict<'key, 'value when 'key: comparison> = Map<'key, 'value>
        type Option<'a> = Microsoft.FSharp.Core.Option<'a>
        type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>
        type StringBuilder = System.Text.StringBuilder
        type Regex = System.Text.RegularExpressions.Regex

        // Gleam-specific types
        [<Struct>]
        type BitArray = BitArray of int64

        type UtfCodepoint = UtfCodepoint of int64

        type Dynamic = Dynamic of obj

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

        type CompileError = { error: string; byte_index: int64 }

        type Uri =
            { scheme: Option<string>
              userinfo: Option<string>
              host: Option<string>
              port: Option<int>
              path: string
              query: Option<string>
              fragment: Option<string> }

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

// Re-implement some core types under the gleam namespace in case they are
// Referenced explicitly from gleam code
// module gleam =
//     type Option<'a> = Microsoft.FSharp.Core.Option<'a>
//     type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>

//     [<GeneralizableValue>]
//     let inline Ok value = Ok value

//     [<GeneralizableValue>]
//     let inline Error value = Error value

//     let Nil = ()
