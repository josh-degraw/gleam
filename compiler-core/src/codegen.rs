use crate::{
    analyse::TargetSupport,
    build::{ErlangAppCodegenConfiguration, Module},
    config::PackageConfig,
    erlang, fsharp,
    io::FileSystemWriter,
    javascript,
    line_numbers::LineNumbers,
    Result,
};
use itertools::Itertools;
use std::fmt::Debug;

use camino::{Utf8Path, Utf8PathBuf};

/// A code generator that creates a .erl Erlang module and record header files
/// for each Gleam module in the package.
#[derive(Debug)]
pub struct Erlang<'a> {
    build_directory: &'a Utf8Path,
    include_directory: &'a Utf8Path,
}

impl<'a> Erlang<'a> {
    pub fn new(build_directory: &'a Utf8Path, include_directory: &'a Utf8Path) -> Self {
        Self {
            build_directory,
            include_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        modules: &[Module],
    ) -> Result<()> {
        for module in modules {
            let erl_name = module.name.replace("/", "@");
            self.erlang_module(&writer, module, &erl_name)?;
            self.erlang_record_headers(&writer, module, &erl_name)?;
        }
        Ok(())
    }

    fn erlang_module<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        let name = format!("{erl_name}.erl");
        let path = self.build_directory.join(&name);
        let line_numbers = LineNumbers::new(&module.code);
        let output = erlang::module(&module.ast, &line_numbers);
        tracing::debug!(name = ?name, "Generated Erlang module");
        writer.write(&path, &output?)
    }

    fn erlang_record_headers<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        for (name, text) in erlang::records(&module.ast) {
            let name = format!("{erl_name}_{name}.hrl");
            tracing::debug!(name = ?name, "Generated Erlang header");
            writer.write(&self.include_directory.join(name), &text)?;
        }
        Ok(())
    }
}

/// A code generator that creates a .app Erlang application file for the package
#[derive(Debug)]
pub struct ErlangApp<'a> {
    output_directory: &'a Utf8Path,
    config: &'a ErlangAppCodegenConfiguration,
}

impl<'a> ErlangApp<'a> {
    pub fn new(output_directory: &'a Utf8Path, config: &'a ErlangAppCodegenConfiguration) -> Self {
        Self {
            output_directory,
            config,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        modules: &[Module],
    ) -> Result<()> {
        fn tuple(key: &str, value: &str) -> String {
            format!("    {{{key}, {value}}},\n")
        }

        let path = self.output_directory.join(format!("{}.app", &config.name));

        let start_module = config
            .erlang
            .application_start_module
            .as_ref()
            .map(|module| tuple("mod", &format!("{{'{}', []}}", module.replace("/", "@"))))
            .unwrap_or_default();

        let modules = modules
            .iter()
            .map(|m| m.name.replace("/", "@"))
            .sorted()
            .join(",\n               ");

        // TODO: When precompiling for production (i.e. as a precompiled hex
        // package) we will need to exclude the dev deps.
        let applications = config
            .dependencies
            .keys()
            .chain(
                config
                    .dev_dependencies
                    .keys()
                    .take_while(|_| self.config.include_dev_deps),
            )
            // TODO: test this!
            .map(|name| self.config.package_name_overrides.get(name).unwrap_or(name))
            .chain(config.erlang.extra_applications.iter())
            .sorted()
            .join(",\n                    ");

        let text = format!(
            r#"{{application, {package}, [
{start_module}    {{vsn, "{version}"}},
    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, []}}
]}}.
"#,
            applications = applications,
            description = config.description,
            modules = modules,
            package = config.name,
            start_module = start_module,
            version = config.version,
        );

        writer.write(&path, &text)
    }
}

#[derive(Debug)]
pub struct FSharpApp<'a> {
    input_dir: &'a Utf8Path,
    output_directory: &'a Utf8PathBuf,
}

impl<'a> FSharpApp<'a> {
    pub fn new(input_dir: &'a Utf8Path, output_directory: &'a Utf8PathBuf) -> Self {
        Self {
            input_dir,
            output_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        modules: &'a [Module],
        generator: &mut fsharp::Generator<'a>,
    ) -> Result<()> {
        let project_file_path = self
            .output_directory
            .join(format!("{}.fsproj", &config.name));

        println!("input_dir: {}", self.input_dir);
        println!("output_directory: {}", self.output_directory);

        // Write prelude file
        let prelude_file_path = self.output_directory.join("gleam_prelude.fs");
        writer.write(&prelude_file_path, fsharp::FSHARP_PRELUDE)?;

        // Write individual module files
        for module in modules {
            let module_file_path = self.output_directory.join(format!("{}.fs", module.name));
            let module_content = generator.render_module(&module.ast, &module.input_path)?;
            writer.write(&module_file_path, &module_content)?;
        }

        let target_framework = config.fsharp.target_framework.as_str();

        // if config.fsharp.target_framework.is_empty() {
        //     "net8.0"
        // } else {
        //     config.fsharp.target_framework.as_str()
        // };

        // TODO: Support conditionally outputting an exe or library
        // Create project file content
        let project_file_content = format!(
            r#"<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>{}</TargetFramework>
    <OutputType>Exe</OutputType>
    <RootNamespace>{}</RootNamespace>
    <IncludeDocumentation>true</IncludeDocumentation>
  </PropertyGroup>

  <ItemGroup Label="Modules">
    <Compile Include="gleam_prelude.fs" />
    {}
    {}
  </ItemGroup>

  <ItemGroup Label="ProjectReferences">
    <!-- TODO: Add support for local project references -->
  </ItemGroup>

  <ItemGroup Label="PackageReferences">
    {}
  </ItemGroup>
</Project>
"#,
            target_framework,
            config.name,
            generator
                .external_files
                .iter()
                .map(|file| format!(
                    "<Compile Include=\"./external/{}\" />",
                    file.file_name().expect("Missing file name")
                ))
                .collect::<Vec<_>>()
                .join("\n    "),
            modules
                .iter()
                .map(|m| format!("<Compile Include=\"{}.fs\" />", m.name))
                .collect::<Vec<_>>()
                .join("\n    "),
            config
                .fsharp
                .package_references
                .iter()
                .map(|(name, version)| format!(
                    "<PackageReference Include=\"{}\" Version=\"{}\" />",
                    name, version
                ))
                .collect::<Vec<_>>()
                .join("\n    ")
        );
        // Write project file
        writer.write(&project_file_path, &project_file_content)?;

        let directory_build_props =
            std::fs::read_to_string(self.input_dir.join("Directory.Build.props"));
        match directory_build_props {
            Ok(props_file) => {
                _ = writer.write(
                    &self.output_directory.join("Directory.Build.props"),
                    &props_file,
                );
            }
            Err(e) => {
                println!("Error reading Directory.Build.props: {}", e);
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeScriptDeclarations {
    None,
    Emit,
}

#[derive(Debug)]
pub struct JavaScript<'a> {
    output_directory: &'a Utf8Path,
    prelude_location: &'a Utf8Path,
    typescript: TypeScriptDeclarations,
    target_support: TargetSupport,
}

impl<'a> JavaScript<'a> {
    pub fn new(
        output_directory: &'a Utf8Path,
        typescript: TypeScriptDeclarations,
        prelude_location: &'a Utf8Path,
        target_support: TargetSupport,
    ) -> Self {
        Self {
            prelude_location,
            output_directory,
            target_support,
            typescript,
        }
    }

    pub fn render(&self, writer: &impl FileSystemWriter, modules: &[Module]) -> Result<()> {
        for module in modules {
            let js_name = module.name.clone();
            if self.typescript == TypeScriptDeclarations::Emit {
                self.ts_declaration(writer, module, &js_name)?;
            }
            self.js_module(writer, module, &js_name)?
        }
        self.write_prelude(writer)?;
        Ok(())
    }

    fn write_prelude(&self, writer: &impl FileSystemWriter) -> Result<()> {
        let rexport = format!("export * from \"{}\";\n", self.prelude_location);
        let prelude_path = &self.output_directory.join("gleam.mjs");

        // This check skips unnecessary `gleam.mjs` writes which confuse
        // watchers and HMR build tools
        if !writer.exists(prelude_path) {
            writer.write(prelude_path, &rexport)?;
        }

        if self.typescript == TypeScriptDeclarations::Emit {
            let rexport = rexport.replace(".mjs", ".d.mts");
            let prelude_declaration_path = &self.output_directory.join("gleam.d.mts");

            // Type decleration may trigger badly configured watchers
            if !writer.exists(prelude_declaration_path) {
                writer.write(prelude_declaration_path, &rexport)?;
            }
        }

        Ok(())
    }

    fn ts_declaration(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
    ) -> Result<()> {
        let name = format!("{js_name}.d.mts");
        let path = self.output_directory.join(name);
        let output = javascript::ts_declaration(&module.ast, &module.input_path, &module.code);
        tracing::debug!(name = ?js_name, "Generated TS declaration");
        writer.write(&path, &output?)
    }

    fn js_module(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
    ) -> Result<()> {
        let name = format!("{js_name}.mjs");
        let path = self.output_directory.join(name);
        let line_numbers = LineNumbers::new(&module.code);
        let output = javascript::module(
            &module.ast,
            &line_numbers,
            &module.input_path,
            &module.code,
            self.target_support,
            self.typescript,
        );
        tracing::debug!(name = ?js_name, "Generated js module");
        writer.write(&path, &output?)
    }
}
