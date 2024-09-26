use crate::assert_fsharp;

#[test]
fn arity_1() {
    assert_fsharp!(
        r#"
pub fn go() {
  use <- pair()
  123
}

fn pair(f) {
  let x = f()
  #(x, x)
}
"#,
    )
}

#[test]
fn arity_2() {
    assert_fsharp!(
        r#"
pub fn go() {
  use <- pair(1.0)
  123
}

fn pair(x, f) {
  let y = f()
  #(x, y)
}
"#,
    )
}

#[test]
fn arity_3() {
    assert_fsharp!(
        r#"
pub fn go() {
  use <- trip(1.0, "")
  123
}

fn trip(x, y, f) {
  let z = f()
  #(x, y, z)
}
"#,
    )
}

#[test]
fn no_callback_body() {
    assert_fsharp!(
        r#"
pub fn go() {
  let thingy = fn(f) { f() }
  use <- thingy()
}
"#
    );
}

#[test]
fn patterns() {
    assert_fsharp!(
        r#"
pub fn go() {
  use Box(x) <- apply(Box(1))
  x
}

type Box(a) {
  Box(a)
}

fn apply(arg, fun) {
  fun(arg)
}
"#
    );
}
