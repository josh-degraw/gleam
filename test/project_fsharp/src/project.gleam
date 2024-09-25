pub type Car {
  Car(make: String, model: String, driver: Person)
}

pub type Person {
  Person(name: String, age: Int)
}

@external(fsharp, "System.Console", "WriteLine")
pub fn println(format: String) -> Nil

@external(fsharp, "./fsharp_custom_behavior.fs", "FSharpCustomBehavior.print_string_and_int")
pub fn print_string_and_int(s: String, i: Int) -> Nil

pub fn main() {
  let car =
    Car(
      make: "Amphicar",
      model: "Model 770",
      driver: Person(name: "John Doe", age: 27),
    )

  println("Hello, world!" <> car.driver.name)
  print_string_and_int("Hello, "<> car.driver.name <> "! Age:", car.driver.age)
  1
}
