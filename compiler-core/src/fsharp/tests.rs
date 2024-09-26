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

mod blocks;
mod case;
mod consts;
mod custom_types;
mod docs;
mod external_fn;
mod functions;
mod numbers;
mod panic;
mod patterns;
mod pipes;
mod records;
mod recursion;
mod strings;
mod todo;
mod use_;
mod variables;

#[macro_export]
macro_rules! assert_fsharp {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output = $crate::fsharp::tests::compile_test_project(
            $src,
            Some(($dep_package, $dep_name, $dep_src)),
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr $(,)?) => {{
        let output = $crate::fsharp::tests::compile_test_project($src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile_test_project(src: &str, dep: Option<(&str, &str, &str)>) -> String {
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
    if let Some((dep_package, dep_name, dep_src)) = dep {
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
    let mut ast = parsed.module;
    ast.name = "my/mod".into();
    let line_numbers = LineNumbers::new(src);
    let ast = crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::FSharp,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, line_numbers, path)
    .expect("should successfully infer root FSharp");

    //println!("AST:{:#?}", &ast);
    let mut generator = crate::fsharp::Generator::new(&ast);
    generator.render().expect("should render FSharp")
}
