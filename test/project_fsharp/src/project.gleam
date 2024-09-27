import gleam.{type Result, Ok, Error}

@external(fsharp, "System.Console", "ReadLine")
pub fn readline() -> String

@external(fsharp, "System.Console", "WriteLine")
pub fn println(format: String) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fs", "FSharpCustomBehavior.print_string_and_int")
pub fn print_string_and_int(s: String, i: Int) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fs", "FSharpCustomBehavior.print_result")
pub fn print_result(s: Result(a,b)) -> Nil


fn say_hello(name){
  "Hello, " <> name
}
pub fn main() {
  println("What is your name?")

  let x = Ok( 1)
  let y = Error (2)
  let z = Ok ("three")
  let a = Error ("four")

  readline()
  |> say_hello
  |> println
  print_string_and_int("This is a number:", 1)
  print_result(z)
  0
}
