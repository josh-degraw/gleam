use camino::Utf8PathBuf;

use crate::{
    analyse::TargetSupport,
    build::{Origin, Target},
    config::PackageConfig,
    line_numbers::LineNumbers,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};

mod bit_arrays;
mod blocks;
mod bools;
mod case;
mod case_clause_guards;
mod consts;
mod custom_types;
mod docs;
mod external_fn;
mod functions;
mod generics;
mod imports;
mod lists;
mod numbers;
mod panic;
mod patterns;
mod pipes;
mod prelude;
mod records;
mod recursion;
mod results;
mod strings;
mod todo;
mod tuples;
mod type_alias;
mod types;
mod use_;
mod variables;
pub static CURRENT_PACKAGE: &str = "thepackage";

#[macro_export]
macro_rules! assert_fsharp {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output = $crate::fsharp::tests::compile_test_project(
            $src,
            vec![($dep_package, $dep_name, $dep_src)],
        )
        .expect("compilation failed");
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr $(,)?) => {{
        let output =
            $crate::fsharp::tests::compile_test_project($src, vec![]).expect("compilation failed");
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

#[macro_export]
macro_rules! assert_fsharp_with_multiple_imports {
    ($(($name:literal, $module_src:literal)),*; $src:literal) => {
        let output =
            $crate::fsharp::tests::compile_test_project($src, vec![$((CURRENT_PACKAGE, $name, $module_src)),*]).expect("compilation failed");
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

#[macro_export]
macro_rules! assert_fsharp_error {
    ($src:expr $(,)?) => {{
        let output = $crate::fsharp::tests::expect_fsharp_error($src, vec![]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

// #[macro_export]
// macro_rules! assert_fsharp_with_import {
//     ($(($name:literal, $module_src:literal)); $src:literal) => {
//         let output =
//             $crate::fsharp::tests::compile_test_project($src, vec![$((CURRENT_PACKAGE, $name, $module_src)),*]);
//         insta::assert_snapshot!(insta::internals::AutoName, output, $src);
//     };
// }

#[track_caller]
pub fn compile_test_project(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
) -> Result<String, crate::Error> {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = std::collections::HashMap::from_iter(vec![]);
    for (dep_package, dep_name, dep_src) in deps {
        let mut dep_config = PackageConfig::default();
        dep_config.name = dep_package.into();
        let parsed = crate::parse::parse_module(
            Utf8PathBuf::from("test/path"),
            dep_src,
            &WarningEmitter::null(),
        )
        .expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = dep_name.into();
        let line_numbers = LineNumbers::new(dep_src);

        let dep = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::FSharp,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &std::collections::HashMap::new(),
            target_support: TargetSupport::NotEnforced,
            package_config: &dep_config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer dep FSharp");
        let _ = modules.insert(dep_name.into(), dep.type_info);
        let _ = direct_dependencies.insert(dep_package.into(), ());
    }

    let path = Utf8PathBuf::from("/root/project/test/my/mod.gleam");
    let parsed = crate::parse::parse_module(path.clone(), src, &WarningEmitter::null())
        .expect("syntax error");
    let mut config = PackageConfig::default();
    config.name = "thepackage".into();
    config.fsharp.type_mappings = std::collections::HashMap::from_iter(vec![]);
    let mut ast = parsed.module;
    ast.name = "my/mod".into();
    let line_numbers = LineNumbers::new(src);
    let module = crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::FSharp,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, line_numbers, path.clone())
    .expect("should successfully infer root FSharp");

    let mut generator =
        crate::fsharp::Generator::new(&config.name, &module, &path, &config.fsharp.type_mappings);
    generator.render()
}

pub fn expect_fsharp_error(src: &str, deps: Vec<(&str, &str, &str)>) -> String {
    let error = compile_test_project(src, deps).expect_err("should not compile");
    println!("er: {error:#?}");
    let better_error = match error {
        crate::Error::FSharp {
            error: inner_error, ..
        } => crate::Error::FSharp {
            src: src.into(),
            path: Utf8PathBuf::from("/src/fsharp/error.gleam"),
            error: inner_error,
        },
        _ => panic!("expected fsharp error, got {error:#?}"),
    };
    better_error.pretty_string()
}
