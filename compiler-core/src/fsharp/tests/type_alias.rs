use crate::assert_fsharp;

#[test]
fn type_alias() {
    assert_fsharp!(
        r#"
pub type Headers = List(#(String, String))
"#,
    );
}

#[test]
fn function_type_alias() {
    assert_fsharp!(
        r#"
pub type Decoder(t) =
  fn(Dynamic) -> Result(t, DecodeErrors)

type Dynamic = String
type DecodeErrors = String
type Result(a,b){
    Ok(a)
    Err(b)
}
"#,
    );
}
