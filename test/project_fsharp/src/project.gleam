
@external(fsharp, "System.Console", "ReadLine")
pub fn readline() -> String

@external(fsharp, "System.Console", "WriteLine")
pub fn println(format: anything) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fsx", "FSharpCustomBehavior.print_string_and_int")
pub fn print_string_and_int(s: String, i: Int) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fsx", "FSharpCustomBehavior.print_result")
pub fn print_result(s: Result(a,b)) -> Nil

type Stack(a)

type SomeCustomType

@external(fsharp, "./fsharp_custom_behavior.fsx", "FSharpCustomBehavior.make_stack")
fn make_stack(a: a) -> Stack(a)

@external(fsharp, "./fsharp_custom_behavior.fsx", "FSharpCustomBehavior.get_some_custom_type")
fn get_some_custom_type() -> SomeCustomType

@external(fsharp, "./fsharp_custom_behavior.fsx", "FSharpCustomBehavior.exit")
fn exit(i: Int) -> a

fn say_hello(name){
  "Hello, " <> name
}
pub fn main() {
  println("What is your name?")

  let s = make_stack(1)
  let t = get_some_custom_type()

  readline()
  |> say_hello
  |> println
  print_string_and_int("This is a number:", 1)
  println(s)
  println(t)
  exit(0)
}
