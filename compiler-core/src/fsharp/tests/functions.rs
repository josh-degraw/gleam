use crate::assert_fsharp;

#[test]
fn function_as_value() {
    assert_fsharp!(
        r#"
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
fn test_function_declaration_and_usage() {
    assert_fsharp!(
        "
fn add(a, b) {a + b}

pub fn foo() {
  add(1,2)
}
"
    );
}

#[test]
fn entry_point_main_function_with_no_args() {
    assert_fsharp!(
        "
pub fn main() {
    0
}
"
    );
}
#[test]
fn entry_point_main_function_with_args() {
    assert_fsharp!(
        "
pub fn main(args) {
    0
}
"
    );
}

#[test]
fn function_with_use_statement() {
    assert_fsharp!(
        r#"
fn go() {
  use a <- something
  a + 1
}
fn something(cba) { cba(1) }
"#
    );
}

#[test]
fn function_with_use_statements() {
    assert_fsharp!(
        r#"
fn go() {
  use a <- something
  use b <- something
  a + 1 + b
}
fn something(cba) { cba(1) }
"#
    );
}

#[test]
fn test_function_with_nested_function() {
    assert_fsharp!(
        r#"
type Option(a) {
  Some(a)
  None
}
fn go(a) {
    let inc_or_zero = fn(x) {
        case x {
            Some(i) -> i + 1
            None -> 0
        }
    }
    inc_or_zero(a)
}
"#
    );
}

#[test]
fn function_call_with_immediate_record_access() {
    assert_fsharp!(
        r#"
type Value {
    Value(content: String)
}

fn get_value(content) {
    Value(content: content)
}

fn go(){
    let x = get_value("hello").content
}

"#
    );
}

// #[test]
// fn anonymous_function_with_single_let_assert() {
//     assert_fsharp!(
//         r#"
// fn each(items, f: fn(a) -> Nil){

// }

// pub fn each_test() {
//   each([1, 1, 1], fn(x) {
//     let assert 1 = x
//   })
// }
// "#
//     );
// }
