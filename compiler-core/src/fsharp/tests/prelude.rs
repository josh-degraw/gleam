use crate::assert_fsharp;

#[test]
fn qualified_ok() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Ok(1) }
"#,
    );
}

#[test]
fn qualified_error() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Error(1) }
"#,
    );
}

#[test]
fn qualified_nil() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Nil }
"#,
    );
}
