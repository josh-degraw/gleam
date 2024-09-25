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

#[test]
fn external_fn_from_file() {
    assert_fsharp!(
        r#"
@external(fsharp, "./SomeFile.fs", "SomeFile.run")
pub fn run() -> Int

pub fn foo() -> Int {
    run()
}
"#
    );
}
