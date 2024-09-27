use crate::assert_fsharp;

#[test]
fn fn_generics() {
    assert_fsharp!(
        r#"pub fn identity(a) -> a {
  a
}
"#,
    );
}

#[test]
fn record_generics() {
    assert_fsharp!(
        r#"pub type Animal(t) {
  Cat(type_: t)
  Dog(type_: t)
}

pub fn go() {
  Cat(type_: 6)
}
"#,
    );
}

#[test]
fn tuple_generics() {
    assert_fsharp!(
        r#"pub fn make_tuple(x: t) -> #(Int, t, Int) {
  #(0, x, 1)
}
"#,
    );
}

#[test]
fn result() {
    assert_fsharp!(
        r#"pub fn map(result, fun) {
            case result {
              Ok(a) -> Ok(fun(a))
              Error(e) -> Error(e)
            }
          }"#,
    );
}
