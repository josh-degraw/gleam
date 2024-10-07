use crate::assert_fsharp;

#[test]
fn basic_battern_matching() {
    assert_fsharp!(
        "
pub fn foo(x) {
  case x {
    1 -> 1
    _ -> 2
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1675
#[test]
fn alternative_pattern_variable_rewriting() {
    assert_fsharp!(
        "
pub fn myfun(mt) {
  case mt {
    1 | _ ->
      1
      |> Ok
  }
  1
  |> Ok
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn positive_zero_pattern() {
    assert_fsharp!(
        "
pub fn foo(x) {
  case x {
    0.0 -> 1
    _ -> 2
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn negative_zero_pattern() {
    assert_fsharp!(
        "
pub fn foo(x) {
  case x {
    -0.0 -> 1
    _ -> 2
  }
}
"
    )
}

#[test]
fn not() {
    assert_fsharp!(
        r#"pub fn foo(x, y) {
  case x {
    _ if !y -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn not_two() {
    assert_fsharp!(
        r#"pub fn foo(x, y) {
  case x {
    _ if !y && !x -> 0
    _ -> 1
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/2657
#[test]
fn spread_empty_list() {
    assert_fsharp!(
        r#"
pub fn foo() {
  case [] {
    [..] -> 1
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/2657
#[test]
fn spread_empty_list_assigning() {
    assert_fsharp!(
        r#"
pub fn foo() {
  case [] {
    [..rest] -> rest
  }
}
"#,
    );
}

#[test]
fn nested_list_pattern() {
    assert_fsharp!(
        r#"
fn match_sequences(
  sequences: List(List(a))
) {
  case sequences {
    [] -> []
    [sequence] -> sequence
    [ascending1, ascending2, ..rest] -> []
  }
}
"#,
    );
}

#[test]
fn nested_list_pattern2() {
    assert_fsharp!(
        r#"
fn match_sequences(
  sequences: List(List(a))
) {
  case sequences {
    [] -> []
    [[a, ..ascending1], ascending2, ..rest] -> []
    [[], ..sequence] -> [sequence]
    _ -> []
  }
}
"#,
    );
}
