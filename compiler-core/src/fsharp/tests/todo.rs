use crate::assert_fsharp;

#[test]
fn plain() {
    assert_fsharp!(
        r#"
pub fn main() {
  todo
}
"#
    );
}

#[test]
fn todo_as() {
    assert_fsharp!(
        r#"
pub fn main() {
  todo as "wibble"
}
"#
    );
}

#[test]
fn todo_as_function() {
    assert_fsharp!(
        r#"
pub fn retstring() {
  "wibble"
}
pub fn main() {
  todo as { retstring() <> "wobble" }
}
"#
    );
}

#[test]
fn piped() {
    assert_fsharp!(
        r#"
     pub fn main() {
      "lets"
      |> todo as "pipe"
      |> todo as "other todo"
    }
    "#
    );
}
