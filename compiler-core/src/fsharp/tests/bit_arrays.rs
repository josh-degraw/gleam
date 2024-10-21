use crate::{assert_fsharp, assert_fsharp_error};

#[test]
fn empty() {
    assert_fsharp!(
        r#"
fn go() {
  <<>>
}
"#,
    );
}

#[test]
fn one() {
    assert_fsharp!(
        r#"
fn go() {
  <<256>>
}
"#,
    );
}

#[test]
fn two() {
    assert_fsharp!(
        r#"
fn go() {
  <<256, 4>>
}
"#,
    );
}

#[test]
fn integer() {
    assert_fsharp!(
        r#"
fn go() {
  <<256:int>>
}
"#,
    );
}

#[test]
fn float() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float>>
}
"#,
    );
}

#[test]
fn float_big_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float-big>>
}
"#,
    );
}

#[test]
fn float_little_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float-little>>
}
"#,
    );
}

#[test]
fn float_sized() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float-32>>
}
"#,
    );
}

#[test]
fn float_sized_big_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float-32-big>>
}
"#,
    );
}

#[test]
fn float_sized_little_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<1.1:float-32-little>>
}
"#,
    );
}

#[test]
fn sized() {
    assert_fsharp!(
        r#"
fn go() {
  <<256:64>>
}
"#,
    );
}

#[test]
fn sized_big_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<256:16-big>>
}
"#,
    );
}

#[test]
fn sized_little_endian() {
    assert_fsharp!(
        r#"
fn go() {
  <<256:16-little>>
}
"#,
    );
}

#[test]
fn explicit_sized() {
    assert_fsharp!(
        r#"
fn go() {
  <<256:size(64)>>
}
"#,
    );
}

#[test]
fn variable_sized() {
    assert_fsharp!(
        r#"
fn go(x, y) {
  <<x:size(y)>>
}
"#,
    );
}

#[test]
fn variable() {
    assert_fsharp!(
        r#"
fn go(x) {
  <<256, 4, x>>
}
"#,
    );
}

#[test]
fn utf8() {
    assert_fsharp!(
        r#"
fn go(x) {
  <<256, 4, x, "Gleam":utf8>>
}
"#,
    );
}

// #[test]
// fn match_utf8() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<"Gleam 👍":utf8>> = x
// }
// "#,
//     );
// }

#[test]
fn utf8_codepoint() {
    assert_fsharp!(
        r#"
fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn bit_string() {
    assert_fsharp!(
        r#"
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits() {
    assert_fsharp!(
        r#"
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

// #[test]
// fn empty_match() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_bytes() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<1, y>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16, b:8>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_unsigned() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:unsigned>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_signed() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:signed>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_big_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-big>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_little_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-little>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_big_endian_unsigned() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-big-unsigned>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_big_endian_signed() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-big-signed>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_little_endian_unsigned() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-little-unsigned>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_little_endian_signed() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:16-little-signed>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_dynamic_size_error() {
//     assert_fsharp_error!(
//         r#"
// fn go(x) {
//   let n = 16
//   let assert <<a:size(n)>> = x
// }
// "#
//     );
// }

// #[test]
// fn match_non_byte_aligned_size_error() {
//     assert_fsharp_error!(
//         r#"
// fn go(x) {
//   let assert <<a:size(7)>> = x
// }
// "#
//     );
// }

// #[test]
// fn discard_sized() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<_:16, _:8>> = x
//   let assert <<_:16-little-signed, _:8>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_sized_value() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<258:16>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_big_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float-big, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_little_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float-little, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_sized() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float-32, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_sized_big_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float-32-big, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_sized_little_endian() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<a:float-32-little, b:int>> = x
// }
// "#,
//     );
// }

// #[test]
// fn match_float_16_bit_error() {
//     assert_fsharp_error!(
//         r#"
// fn go(x) {
//   let assert <<a:float-size(16)>> = x
// }
// "#
//     );
// }

// #[test]
// fn match_rest() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<_, b:bytes>> = <<1,2,3>>
// }
// "#,
//     );
// }

// #[test]
// fn match_rest_deprecated() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<_, b:bytes>> = <<1,2,3>>
// }
// "#,
//     );
// }

// #[test]
// fn match_binary_size() {
//     assert_fsharp!(
//         r#"
// fn go(x) {
//   let assert <<_, a:2-bytes>> = x
//   let assert <<_, b:bytes-size(2)>> = x
// }
// "#,
//     );
// }

#[test]
fn as_module_const() {
    assert_fsharp!(
        r#"
          pub const data = <<
            0x1,
            2,
            2:size(16),
            0x4:size(32),
            -1:32,
            "Gleam":utf8,
            4.2:float,
            4.2:32-float,
            <<
              <<1, 2, 3>>:bits,
              "Gleam":utf8,
              1024
            >>:bits
          >>
        "#
    )
}

#[test]
fn negative_size() {
    assert_fsharp!(
        r#"
fn go() {
  <<1:size(-1)>>
}
"#,
    );
}

// // https://github.com/gleam-lang/gleam/issues/1591
// #[test]
// fn not_byte_aligned() {
//     assert_fsharp_error!(
//         r#"
// fn thing() {
//   4
// }

// fn go() {
//   <<256:4>>
// }
// "#,
//     );
// }

// #[test]
// fn not_byte_aligned_explicit_sized() {
//     assert_fsharp_error!(
//         r#"
// fn go() {
//   <<256:size(4)>>
// }
// "#,
//     );
// }

// This test would ideally also result in go() being deleted like the previous tests
// but we can not know for sure what the value of a variable is going to be
// so right now go() is not deleted.
#[test]
fn not_byte_aligned_variable() {
    assert_fsharp!(
        r#"
fn go() {
  let x = 4
  <<256:size(x)>>
}
"#,
    );
}

#[test]
fn bit_array_literal_string_constant_is_treated_as_utf8() {
    assert_fsharp!(r#"const a = <<"hello", " ", "world">>"#);
}

#[test]
fn bit_array_literal_string_is_treated_as_utf8() {
    assert_fsharp!(
        r#"
pub fn go() {
  <<"hello", " ", "world">>
}"#
    );
}

// #[test]
// fn bit_array_literal_string_pattern_is_treated_as_utf8() {
//     assert_fsharp!(
//         r#"
// pub fn go() {
//   case <<>> {
//     <<"a", "b", _:bytes>> -> 1
//     _ -> 2
//   }
// }"#
//     );
// }
