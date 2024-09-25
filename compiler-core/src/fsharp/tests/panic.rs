use crate::assert_fsharp;

#[test]
fn panic_as() {
    assert_fsharp!(
        r#"
pub fn foo() {
  panic as "wibble"
}
"#
    );
}

#[test]
fn plain() {
    assert_fsharp!(
        r#"
pub fn foo() {
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
pub fn foo() {
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
pub fn foo() {
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
     pub fn foo() {
      "lets"
      |> panic as "pipe"
      |> panic as "other panic"
    }
    "#
    );
}
