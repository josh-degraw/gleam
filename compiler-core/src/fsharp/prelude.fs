[<AutoOpen>]
module ``gleam prelude``

let (|Gleam__codegen__prefix|_|) (p: string) (s: string) : string option =
  if s.StartsWith(p) then
    Some(s.Substring(p.Length))
  else
    None
