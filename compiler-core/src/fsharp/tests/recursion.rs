use crate::assert_fsharp;

#[test]
fn tco() {
    assert_fsharp!(
        r#"
pub fn foo(x) {
  case x {
    0 -> Nil
    _ -> foo(x - 1)
  }
}
"#
    );
}

#[test]
fn tco_case_block() {
    assert_fsharp!(
        r#"
pub fn foo(x) {
  case x {
    0 -> Nil
    _ -> {
      let y = x
      foo(y - 1)
    }
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1779
#[test]
fn not_tco_due_to_assignment() {
    assert_fsharp!(
        r#"
pub fn foo(x) {
  let z = {
    let y = x
    foo(y - 1)
  }
  z
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2400
#[test]
fn shadowing_so_not_recursive() {
    // This funtion is calling an argument with the same name as itself, so it is not recursive
    assert_fsharp!(
        r#"
pub fn map(map) {
  map()
}
"#
    );
}
