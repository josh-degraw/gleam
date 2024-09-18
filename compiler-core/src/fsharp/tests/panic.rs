use crate::assert_fsharp;

#[test]
fn panic_as() {
    assert_fsharp!(
        r#"
pub fn main() {
  panic as "wibble"
}
"#
    );
}

#[test]
fn plain() {
    assert_fsharp!(
        r#"
pub fn main() {
  panic
}
"#
    );
}

#[test]
fn panic_as_function() {
    assert_fsharp!(
        r#"
pub fn retstring() {
  "wibble"
}
pub fn main() {
  panic as { retstring() <> "wobble" }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2176
#[test]
fn piped() {
    assert_fsharp!(
        r#"
pub fn main() {
  "lets"
  |> panic
}
    "#
    );
}

#[test]
fn piped_chain() {
    assert_fsharp!(
        r#"
     pub fn main() {
      "lets"
      |> panic as "pipe"
      |> panic as "other panic"
    }
    "#
    );
}
