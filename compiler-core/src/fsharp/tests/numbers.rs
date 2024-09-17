use crate::assert_fsharp;

#[test]
fn simple_variable_assignment() {
    assert_fsharp!(
        r#"
pub fn main() {
  let a = 3
}
"#
    );
}
