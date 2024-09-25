use crate::assert_fsharp;

#[test]
fn function_as_value() {
    assert_fsharp!(
        r#"
fn other() {
  Nil
}

pub fn foo() {
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

pub fn foo() {
  add(1,2)
}
"
    );
}

#[test]
fn entry_point_main_function_with_no_args() {
    assert_fsharp!(
        "
pub fn main() {
    0
}
"
    );
}
#[test]
fn entry_point_main_function_with_args() {
    assert_fsharp!(
        "
pub fn main(args) {
    0
}
"
    );
}
