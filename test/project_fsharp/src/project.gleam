pub type Car {
  Car(make: String, model: String, driver: Person)
}

pub type Person {
  Person(name: String, age: Int)
}

@external(fsharp, "System.Console", "WriteLine")
pub fn printfn(format: String) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fs", "FSharpCustomBehavior.print_string_and_int")
pub fn print_string_and_int(s: String, i: Int) -> Nil

pub fn main(args: List(String)) {
  let car =
    Car(
      make: "Amphicar",
      model: "Model 770",
      driver: Person(name: "John Doe", age: 27),
    )
  let new_p = Person(..car.driver, age: 28)
  new_p

  printfn("Hello, world!" <> car.driver.name)
  1
}
