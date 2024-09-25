

@external(fsharp, "System.Console", "ReadLine")
pub fn readline() -> String

@external(fsharp, "System.Console", "WriteLine")
pub fn println(format: String) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fs", "FSharpCustomBehavior.print_string_and_int")
pub fn print_string_and_int(s: String, i: Int) -> Nil


fn say_hello(name){
  "Hello, " <> name
}
pub fn main() {
  println("What is your name?")

  readline()
  |> say_hello
  |> println
  print_string_and_int("This is a number:", 1)
  0
}
