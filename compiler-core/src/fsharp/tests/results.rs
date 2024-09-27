use crate::assert_fsharp;

#[test]
fn ok() {
    assert_fsharp!(r#"pub fn go() { Ok(1) }"#);
}

#[test]
fn error() {
    assert_fsharp!(r#"pub fn go() { Error(1) }"#);
}

#[test]
fn ok_fn() {
    assert_fsharp!(r#"pub fn go() { Ok }"#);
}

#[test]
fn error_fn() {
    assert_fsharp!(r#"pub fn go() { Error }"#);
}

#[test]
fn qualified_ok() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Ok(1) }"#
    );
}

#[test]
fn qualified_error() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Error(1) }"#
    );
}

#[test]
fn qualified_ok_fn() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Ok }"#
    );
}

#[test]
fn qualified_error_fn() {
    assert_fsharp!(
        r#"import gleam
pub fn go() { gleam.Error }"#
    );
}

#[test]
fn aliased_ok() {
    assert_fsharp!(
        r#"import gleam.{Ok as Thing}
pub fn go() { Thing(1) }"#
    );
}

#[test]
fn aliased_error() {
    assert_fsharp!(
        r#"import gleam.{Error as Thing}
pub fn go() { Thing(1) }"#
    );
}

#[test]
fn aliased_ok_fn() {
    assert_fsharp!(
        r#"import gleam.{Ok as Thing}
pub fn go() { Thing }"#
    );
}

#[test]
fn aliased_error_fn() {
    assert_fsharp!(
        r#"import gleam.{Error as Thing}
pub fn go() { Thing }"#
    );
}
