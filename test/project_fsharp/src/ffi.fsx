module rec my.``mod``

let go (x: string) : string =
  begin
    match x with
    | Gleam__codegen__prefix "Hello, " name -> name
    | _ -> "Unknown"
  end

let go (x: string) : string =
  begin
    match x with
    | ``gleam Prefix`` "Hello, " name when name = "Dude" -> name
    | _ -> "Unknown"
  end
