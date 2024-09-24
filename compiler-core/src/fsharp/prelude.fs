[<AutoOpen>]
module ``gleam prelude``

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
