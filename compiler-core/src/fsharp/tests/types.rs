use crate::{assert_fsharp, assert_fsharp_with_multiple_imports};

#[test]
fn type_inferrence_produces_correct_type_params() {
    assert_fsharp!(
        r#"
type Action(element) {
  Stop
  Continue(element, fn() -> Action(element))
}
pub opaque type Iterator(element) {
  Iterator(continuation: fn() -> Action(element))
}
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}
pub fn repeatedly(f: fn() -> element) -> Iterator(element) {
 todo
}
 pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Iterator(element) {
  todo
}
"#,
    );
}

#[test]
fn sanitizes_type_var() {
    assert_fsharp!(
        r#"
pub fn foo(class: class, member: member) {member}
"#,
    );
}
#[test]
fn uses_correct_record_constructor() {
    assert_fsharp!(
        (
            "",
            "gleam/iterator",
            r#"
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}
"#
        ),
        r#"
import gleam/iterator.{Next}
type Cat {
  Cat(id: Int)
}
fn go(cat: Cat) {
    Next(cat, Cat(id: cat.id + 1))
    iterator.Next(cat, Cat(id: cat.id + 1))
}
"#,
    );
}
