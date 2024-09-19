module rec my.``mod``
(**
```gleam
fn get_name(x: String) -> String {
  case x {
    "Hello, " <> name -> name
    _ -> "Unknown"
  }
}
```
*)

let get_name(x:string) =
  match x with
  | "Hello, "
