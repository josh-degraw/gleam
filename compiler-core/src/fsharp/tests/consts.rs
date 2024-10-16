use crate::assert_fsharp;

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor() {
    assert_fsharp!(
        r#"
pub type X {
  X(Int)
}

pub const z = X

pub fn foo() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor_in_tuple() {
    assert_fsharp!(
        r#"
pub type X {
  X(Int)
}

pub const z = #(X)

pub fn foo() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2179
#[test]
fn const_type_variable() {
    assert_fsharp!(
        r#"
fn identity(a: a) -> a {
  a
}

const id: fn(a) -> a = identity
"#
    );
}

#[test]
fn const_generalise() {
    assert_fsharp!(
        r#"
fn identity(a: a) -> a {
a
}

const id  = identity

pub fn foo(){
  let num  = id(1)
  let word = id("Word")
}"#
    );
}

#[test]
fn pub_const_equal_to_private_function() {
    assert_fsharp!(
        r#"
          fn identity(a) {
            a
          }

          pub const id = identity
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_private_function_field() {
    assert_fsharp!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_nested_private_function_field() {
    assert_fsharp!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub type Funcs(b) {
            Funcs(mapper: Mapper(b))
          }

          pub const id_mapper = Funcs(Mapper(identity))
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_private_function() {
    assert_fsharp!(
        r#"
              fn identity(a) {
                a
              }

              pub const id = identity
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_record_with_private_function_field() {
    assert_fsharp!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_qualified_pub_const_equal_to_record_with_private_function_field() {
    assert_fsharp!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_private_in_internal() {
    assert_fsharp!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              @internal
              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_private_in_list() {
    assert_fsharp!(
        r#"
          fn identity(a) {
            a
          }

          pub const funcs = [identity]
        "#
    )
}

#[test]
fn use_private_in_tuple() {
    assert_fsharp!(
        r#"
          fn identity(a) {
            a
          }

          pub const funcs = #(identity)
        "#
    )
}
