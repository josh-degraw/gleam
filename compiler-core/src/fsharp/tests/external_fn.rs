use crate::assert_fsharp;

#[test]
fn external_fn_from_std_lib() {
    assert_fsharp!(
        r#"
@external(fsharp, "FSharp.MyApp", "run")
pub fn run() -> Int

pub fn foo() -> Int {
    run()
}
"#
    );
}

// #[test]
// fn external_fn_from_file() {
//     assert_fsharp!(
//         r#"
// @external(fsharp, "./test-file.fsx", "TestFile.run")
// pub fn run() -> Int

// pub fn foo() -> Int {
//     run()
// }
// "#
//     );
// }

// #[test]
// fn external_type() {
//     assert_fsharp!(
//         r#"
// type ExternalType
// "#
//     );
// }
// #[test]
// fn generic_external_type() {
//     assert_fsharp!(
//         r#"
// type ExternalType
// "#
//     );
// }
