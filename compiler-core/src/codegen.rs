use crate::{
    analyse::TargetSupport,
    build::{ErlangAppCodegenConfiguration, Module},
    config::{FSharpOutputType, FSharpTestFramework, PackageConfig},
    erlang, fsharp,
    io::FileSystemWriter,
    javascript,
    line_numbers::LineNumbers,
    requirement::Requirement,
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
    config: &'a PackageConfig,
}

impl<'a> FSharpApp<'a> {
    pub fn new(
        config: &'a PackageConfig,
        input_dir: &'a Utf8Path,
        output_directory: &'a Utf8PathBuf,
    ) -> Self {
        Self {
            config,
            input_dir,
            output_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        modules: &'a [Module],
        generator: &mut fsharp::Generator<'a>,
    ) -> Result<()> {
        // Write prelude file
        let prelude_file_path = self.output_directory.join("gleam_prelude.fs");
        writer.write(&prelude_file_path, fsharp::FSHARP_PRELUDE)?;

        // Write gleam.toml
        let gleam_toml_path = self.input_dir.join("../gleam.toml");
        let existing_gleam_toml = std::fs::read_to_string(&gleam_toml_path)
            .expect(&format!("Failed to read {gleam_toml_path}"));
        let output_gleam_toml_path = self.output_directory.join("gleam.toml");
        writer.write(&output_gleam_toml_path, &existing_gleam_toml)?;

        // Write individual module files
        for module in modules {
            let module_file_path = if module.is_test() {
                self.output_directory.join(format!("{}.fs", module.name))
            } else {
                self.output_directory.join(format!("{}.fs", module.name))
            };
            let module_content = generator.render_module(module, &module.input_path)?;
            writer.write(&module_file_path, &module_content)?;
        }

        // TODO: Support conditionally outputting an exe or library
        // Create project file content
        self.write_project_file(
            &writer,
            modules.iter().filter(|m| !m.is_test()),
            generator,
            false,
        )?;

        self.write_project_file(&writer, modules.iter(), generator, true)?;

        Ok(())
    }

    fn write_project_file<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        modules: impl Iterator<Item = &'a Module>,
        generator: &mut fsharp::Generator<'a>,
        test_project: bool,
    ) -> Result<()> {
        let modules = modules.collect::<Vec<_>>();
        if modules.is_empty() {
            return Ok(());
        }

        let project_file_path = if test_project {
            self.output_directory
                .join(format!("{}_test.fsproj", &self.config.name))
        } else {
            self.output_directory
                .join(format!("{}.fsproj", &self.config.name))
        };

        let external_files = generator
            .external_files
            .iter()
            .filter(|file| {
                if !test_project {
                    !file.to_string().contains("test")
                } else {
                    true
                }
            })
            .map(|file| {
                format!(
                    "<Compile Include=\"./external/{}\" />",
                    file.file_name().expect("Missing file name")
                )
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        let source_files = modules
            .iter()
            .map(|m| {
                format!(
                    "<Compile Include=\"{}.fs\" />",
                    self.output_directory.join(m.name.to_string())
                )
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        let project_references = self
            .config
            .dependencies
            .iter()
            .map(|(name, requirement)| match requirement {
                Requirement::Path { path } => {
                    let project_name = path
                        .as_str()
                        .split("/")
                        .last()
                        .expect("Missing dependency project name");

                    format!(
                        "<ProjectReference Include=\"../{}/_gleam_artefacts/{}.fsproj\" />",
                        path.as_str(),
                        project_name
                    )
                }
                _ => format!("<ProjectReference Include=\"{}\" />", name),
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        let package_references = self
            .config
            .fsharp
            .package_references
            .iter()
            .map(|(name, version)| {
                format!(
                    "<PackageReference Include=\"{}\" Version=\"{}\" />",
                    name, version
                )
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        let root_namespace = &self.config.name;

        let target_framework = self.config.fsharp.target_framework.as_str();

        let sdk = "Microsoft.NET.Sdk";

        let package_references = if test_project {
            match &self.config.fsharp.test_framework {
                FSharpTestFramework::XUnit {
                    version,
                    runner_version,
                } => {
                    format!(
                        "{package_references}
    <PackageReference Include=\"Microsoft.NET.Test.Sdk\" Version=\"17.11.0\" />
    <PackageReference Include=\"xunit\" Version=\"{version}\" />
    <PackageReference Include=\"xunit.runner.visualstudio\" Version=\"{runner_version}\">
        <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
        <PrivateAssets>all</PrivateAssets>
    </PackageReference>"
                    )
                }
            }
        } else {
            package_references
        };

        let additional_properties = if test_project {
            "<IsPackable>false</IsPackable>"
        } else if self.config.fsharp.output_type == FSharpOutputType::Exe {
            "<OutputType>Exe</OutputType>"
        } else {
            "<OutputType>Library</OutputType>"
        };

        let prelude_file_path = self.output_directory.join("gleam_prelude.fs");

        let project_file_content = format!(
            r#"<Project Sdk="{sdk}">
  <PropertyGroup>
    <TargetFramework>{target_framework}</TargetFramework>
    <RootNamespace>{root_namespace}</RootNamespace>
    <IncludeDocumentation>true</IncludeDocumentation>
    <NoWarn>$(NoWarn);FS0020;</NoWarn>
    {additional_properties}
  </PropertyGroup>
  <ItemGroup>
    <None Include="gleam.toml" />
  </ItemGroup>
  <ItemGroup Label="Modules">
    <Compile Include="{prelude_file_path}" />
    {external_files}
    {source_files}
  </ItemGroup>
  <ItemGroup Label="ProjectReferences">
    {project_references}
  </ItemGroup>
  <ItemGroup Label="PackageReferences">
    {package_references}
  </ItemGroup>
</Project>
"#
        );

        writer.write(&project_file_path, &project_file_content)
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
            let rexport = format!(
                "export * from \"{}\";\nexport type * from \"{}\";\n",
                self.prelude_location,
                self.prelude_location.as_str().replace(".mjs", ".d.mts")
            );
            let prelude_declaration_path = &self.output_directory.join("gleam.d.mts");

            // Type declaration may trigger badly configured watchers
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
