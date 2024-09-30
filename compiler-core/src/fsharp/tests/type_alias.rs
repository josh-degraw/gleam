use crate::assert_fsharp;

#[test]
fn type_alias() {
    assert_fsharp!(
        r#"
pub type Headers = List(#(String, String))
"#,
    );
}
