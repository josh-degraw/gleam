[<AutoOpen>]
module ``gleam prelude``

type Regex = System.Text.RegularExpressions.Regex
type Dict<'key, 'value when 'key: comparison> = Map<'key, 'value>

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
module gleam =
    type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>

    [<GeneralizableValue>]
    let inline Ok value = Ok value

    [<GeneralizableValue>]
    let inline Error value = Error value

    let Nil = ()
