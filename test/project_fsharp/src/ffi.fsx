module rec my.``mod``

let go (x: string) : string =
  begin
    match x with
    | Gleam__codegen__prefix "Hello, " name -> name
    | _ -> "Unknown"
  end

let (|Gleam__codegen__prefix|_|) (p: string) (s: string) =
  if s.StartsWith(p) then
    Some(s.Substring(p.Length))
  else
    None
