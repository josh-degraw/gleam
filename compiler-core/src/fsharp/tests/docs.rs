use crate::assert_fsharp;

#[test]
fn single_line_doc_comment() {
    assert_fsharp!(
        r#"
/// This is a doc comment
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
fn multi_line_doc_comment() {
    assert_fsharp!(
        r#"
/// This is a doc comment
/// on multiple lines
fn other() {
  Nil
}

pub fn foo() {
  other()
}
"#
    );
}

#[test]
fn deprecated_function_with_doc_comment() {
    assert_fsharp!(
        r#"
/// This is a doc comment
/// on multiple lines
@deprecated("Use new_function instead")
fn other() {
  Nil
}

pub fn foo() {
  other()
}
"#
    );
}
