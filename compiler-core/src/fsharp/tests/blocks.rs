use crate::assert_fsharp;

#[test]
fn block_ending_in_assignment() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    1
    2
    let y = 3
  }
  x
}
"#,
    );
}

#[test]
fn block() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    1
    2
  }
  x
}
"#,
    );
}

#[test]
fn nested_simple_blocks() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    {
      3
    }
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    1
    {
      2
      3
    }
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks_with_pipe() {
    assert_fsharp!(
        r#"
fn add1(a) {
  a + 1
}
fn go() {
  let x = {
    1
    {
      2
      3 |> add1
    } |> add1
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_non_ending_blocks() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    1
    {
      2
      3
    }
    4
  }
  x
}
"#,
    );
}

#[test]
fn nested_multiexpr_blocks_with_case() {
    assert_fsharp!(
        r#"
fn go() {
  let x = {
    1
    {
      2
      case True {
        _ -> 3
      }
    }
  }
  x
}
"#,
    );
}

#[test]
fn sequences() {
    assert_fsharp!(
        r#"
fn go() {
  "one"
  "two"
  "three"
}
"#,
    );
}

#[test]
fn left_operator_sequence() {
    assert_fsharp!(
        r#"
fn go() {
  1 == {
    1
    2
  }
}
"#,
    );
}

#[test]
fn right_operator_sequence() {
    assert_fsharp!(
        r#"
fn go() {
  {
    1
    2
  } == 1
}
"#,
    );
}

#[test]
fn concat_blocks() {
    assert_fsharp!(
        r#"
fn foo(f, a, b) {
  {
    a
    |> f
  } <> {
    b
    |> f
  }
}
"#,
    );
}

#[test]
fn blocks_returning_functions() {
    assert_fsharp!(
        r#"
fn b() {
  {
    fn(cb) { cb(1) }
  }
  {
    fn(cb) { cb(2) }
  }
  3
}
"#
    );
}

#[test]
fn blocks_returning_use() {
    assert_fsharp!(
        r#"
fn b() {
  {
    use a <- fn(cb) { cb(1) }
    a
  }
  {
    use b <- fn(cb) { cb(2) }
    b
  }
  3
}
    "#
    );
}

#[test]
fn block_with_parenthesised_expression_returning_from_function() {
    assert_fsharp!(
        r#"
fn b() {
  {
    1 + 2
  }
}
"#
    );
}
