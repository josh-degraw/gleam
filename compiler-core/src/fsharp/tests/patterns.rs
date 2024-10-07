use crate::assert_fsharp;

#[test]
fn alternative_patterns() {
    // reassigning name in alternative patterns
    assert_fsharp!(
        r#"
pub fn foo() {
  let duplicate_name = 1

  case 1 {
    1 | 2 -> {
      let duplicate_name = duplicate_name + 1
      duplicate_name
    }
    _ -> 0
  }
}"#
    );
}

#[test]
fn alternative_patterns1() {
    // Alternative patterns with a clause containing vars
    assert_fsharp!(
        r#"
pub fn foo() {
  case Ok(1) {
    Ok(duplicate_name) | Error(duplicate_name) -> duplicate_name
  }
}"#
    );
}

#[test]
fn alternative_patterns2() {
    // Alternative patterns with a guard clause containing vars
    assert_fsharp!(
        r#"
pub fn foo() {
    let duplicate_name = 1

    case 1 {
        1 | 2 if duplicate_name == 1 -> duplicate_name
        _ -> 0
    }
}"#
    );
}

#[test]
fn alternative_patterns3() {
    assert_fsharp!(
        r#"
pub const constant = Ok(1)

pub fn foo(arg) {
  let _ = constant
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn pattern_as() {
    assert_fsharp!(
        "pub fn a(x) {
  case x {
    Ok(1 as y) -> 1
    _ -> 0
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_multiple_subjects() {
    assert_fsharp!(
        "pub fn a(x) {
  case x, x {
    _, \"a\" as a <> _  -> a
    _, _ -> \"a\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_multiple_subjects_and_guard() {
    assert_fsharp!(
        "pub fn a(x) {
  case x, x {
    _, \"a\" as a <> rest if rest == \"a\" -> a
    _, _ -> \"a\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_list() {
    assert_fsharp!(
        "pub fn a(x) {
  case x {
    [\"a\" as a <> _, \"b\" as b <> _] -> a <> b
    _ -> \"\"
  }
}"
    );
}

#[test]
fn string_prefix_as_pattern_with_assertion() {
    assert_fsharp!(
        "pub fn a(x) {
  let assert \"a\" as a <> rest = \"wibble\"
  a
}"
    );
}

#[test]
fn assert_pattern_as_last_statement() {
    assert_fsharp!(
        r#"
pub fn go() {
  let assert Error(b) = Error(1)
}
fn int_bind(x) {
    let assert 1 = x
}
pub type Result(a, b) {
  Ok(a)
  Error(b)
}
"#
    );
}

#[test]
fn matching_on_record_with_multiple_constructors() {
    assert_fsharp!(
        "
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}

fn go() {
  case [] {
      [] ->
        step
        |> equal(Done)

      [h, ..t] -> {
        let assert Next(h2, t2) = step
        h
        |> equal(h2)
        t2
        |> equal(t)
      }
    }
}

fn equal(a, b) {
  a == b
}
"
    );
}
