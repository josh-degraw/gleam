use crate::assert_fsharp;

#[test]
fn function_as_value() {
    assert_fsharp!(
        r#"
fn other() {
  Nil
}

pub fn main() {
  other
}
"#
    );
}

#[test]
fn test_function_declaration_and_usage() {
    assert_fsharp!(
        "
fn add(a, b) {a + b}

pub fn main() {
  add(1,2)
}
"
    );
}
