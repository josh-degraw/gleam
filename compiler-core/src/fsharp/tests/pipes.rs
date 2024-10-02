use crate::assert_fsharp;

#[test]
fn simple_pipeline() {
    assert_fsharp!(
        r#"
pub fn add(x, y) { x + y }

pub fn foo() {
  1 |> add(2)
}"#
    );
}

#[test]
fn chained_pipeline() {
    assert_fsharp!(
        r#"
pub fn add(x, y) { x + y }

pub fn foo() {
  1 |> add(2) |> add(3)
}"#
    );
}

#[test]
fn clever_pipe_rewriting() {
    // a |> b
    assert_fsharp!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#
    );
}

#[test]
fn clever_pipe_rewriting1() {
    // a |> b(c)
    assert_fsharp!(
        r#"
pub fn apply(f: fn(a, Int) -> b, a: a) { a |> f(1) }
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/952
#[test]
fn block_expr_into_pipe() {
    assert_fsharp!(
        r#"fn id(a) { a }
pub fn foo() {
  {
    let x = 1
    x
  }
  |> id
}"#
    );
}

#[test]
fn pipe_in_list() {
    assert_fsharp!(
        "pub fn x(f) {
  [
    1 |> f
  ]
}"
    );
}

#[test]
fn pipe_in_tuple() {
    assert_fsharp!(
        "pub fn x(f) {
  #(
    1 |> f
  )
}"
    );
}

#[test]
fn pipe_in_case_subject() {
    assert_fsharp!(
        "pub fn x(f) {
  case 1 |> f {
    x -> x
  }
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1379
#[test]
fn pipe_in_spread() {
    assert_fsharp!(
        "pub type X {
  X(a: Int, b: Int)
}

fn id(x) {
  x
}

pub fn foo(x) {
  X(..x, a: 1 |> id)
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1385
#[test]
fn pipe_in_eq() {
    assert_fsharp!(
        "fn id(x) {
  x
}

pub fn foo() {
    1 == 1 |> id
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1863
#[test]
fn call_pipeline_result() {
    assert_fsharp!(
        r#"
pub fn foo() {
  { 1 |> add }(1)
}

pub fn add(x) {
  fn(y) { x + y }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1931
#[test]
fn pipe_in_call() {
    assert_fsharp!(
        r#"
pub fn foo() {
  123
  |> two(
    1 |> two(2),
    _,
  )
}

pub fn two(a, b) {
  a
}
"#
    );
}

/*
  and(True, True)
  |> is_true

  and(False, True)
  |> is_false

  True
  |> and(True)
  |> is_true

*/
#[test]
fn pipe_in_different_positions() {
    assert_fsharp!(
        r#"
fn and(a, b) {
  a && b
}

fn is_false(a) {a}

pub fn something() {

  False
  |> and(True, _)
  |> is_false
}
"#
    )
}

// #[test]
// fn pipe_in_different_positions1() {
//     assert_fsharp!(
//         r#"

// pub fn upsert(
//     dict,
//     key,
//     f,
// ) {
//   dict
//   |> get(key)
//   |> from_result
//   |> f
//   |> insert(dict, key, _)
// }

// fn get(dict, key) {
//   Ok(1)
// }

// fn insert(dict, key, value) {
//   Nil
// }
// fn from_result(result) {
//   result
// }
// "#
//     );
// }

#[test]
fn multi_argument_pipe_call() {
    assert_fsharp!(
        r#"
fn go(dict, key) {
    "something"
    |> insert(dict, key, _)
}

fn insert(dict, key, value) {
    value
}
"#
    );
}
