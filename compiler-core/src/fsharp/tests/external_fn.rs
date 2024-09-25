use crate::assert_fsharp;

#[test]
fn integration_test1_3() {
    assert_fsharp!(
        r#"
@external(fsharp, "FSharp.MyApp", "run")
pub fn run() -> Int

pub fn main() -> Int {
    run()
}
"#
    );
}
