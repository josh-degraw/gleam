use crate::{
    ast::*,
    config::PackageConfig,
    docvec,
    io::FileSystemWriter,
    pretty::*,
    type_::{ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant},
};
use ecow::EcoString;
use itertools::Itertools;
use std::sync::Arc;
use vec1::Vec1;

pub struct FSharp<'a> {
    build_dir: &'a camino::Utf8Path,
}

impl<'a> FSharp<'a> {
    pub fn new(build_dir: &'a camino::Utf8Path) -> Self {
        Self { build_dir }
    }

    pub fn module(&self, module: &TypedModule) -> Document<'_> {
        let mut docs = Vec::new();

        // Add module declaration
        docs.push(self.module_declaration(module));

        // Add imports
        // docs.extend(self.imports(module));

        // Add type definitions
        docs.extend(self.type_definitions(module));

        // Add function definitions
        docs.extend(self.function_definitions(module));

        docvec![docs.iter().map(|d| d.to_doc()).intersperse(line())]
    }

    fn module_declaration(&self, module: &TypedModule) -> Document<'_> {
        docvec!["module ", module.name.replace("/", ".")]
    }

    // fn imports(&self, module: &TypedModule) -> Vec<Document<'_>> {
    //     module
    //         .
    //         .iter()
    //         .map(|import| {
    //             let module_name = import.module.replace("/", ".");
    //             docvec!["open ", module_name]
    //         })
    //         .collect()
    // }

    fn type_definitions(&self, module: &TypedModule) -> Vec<Document<'_>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::CustomType(t) => Some(self.custom_type(t)),
                Definition::TypeAlias(t) => Some(self.type_alias(t)),
                _ => None,
            })
            .collect()
    }

    fn custom_type(&self, t: &CustomType<Arc<Type>>) -> Document<'_> {
        let name = &t.name;
        let constructors = t
            .constructors
            .iter()
            .map(|c| {
                let fields = c
                    .arguments
                    .iter()
                    .map(|f| self.type_to_fsharp(&f.type_))
                    .collect::<Vec<_>>();
                if fields.is_empty() {
                    docvec![c.name.as_str()]
                } else {
                    docvec![
                        c.name.as_str(),
                        " of ",
                        fields.iter().intersperse(&" * ".to_doc())
                    ]
                }
            })
            .collect::<Vec<_>>();

        docvec![
            "type ",
            name,
            " =",
            line(),
            docvec![constructors
                .iter()
                .map(|c| docvec!["| ", c])
                .intersperse(line())]
        ]
    }

    fn type_alias(&self, t: &TypeAlias<Arc<Type>>) -> Document<'_> {
        docvec![
            "type ",
            t.alias.as_str(),
            " = ",
            self.type_to_fsharp(&t.type_)
        ]
    }

    fn function_definitions(&self, module: &TypedModule) -> Vec<Document<'_>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::Function(f) => Some(self.function(f)),
                _ => None,
            })
            .collect()
    }

    fn function(&self, f: &Function<Arc<Type>, TypedExpr>) -> Document<'_> {
        let name = &f.name;
        let args = f
            .arguments
            .iter()
            .map(|arg| {
                docvec![
                    arg.names
                        .get_variable_name()
                        .unwrap_or_else(|| &EcoString::from("_")),
                    ": ",
                    self.type_to_fsharp(&arg.type_)
                ]
            })
            .collect::<Vec<_>>();
        let return_type = self.type_to_fsharp(&f.return_type);
        let body = f.body.into_iter().map(|b| self.statement(&b)).collect();

        docvec![
            "let ",
            name,
            " ",
            args.iter().intersperse(&" ".to_doc()),
            ": ",
            return_type,
            " =",
            line(),
            body
        ]
    }

    fn statements(&self, statements: &Vec1<Statement<Arc<Type>, TypedExpr>>) -> Document<'_> {
        statements
            .iter()
            .map(|s| self.statement(s))
            .collect::<Vec<_>>()
            .to_doc()
    }

    fn statement(&self, statement: &Statement<Arc<Type>, TypedExpr>) -> Document<'_> {
        match statement {
            Statement::Expression(expr) => self.expression(expr),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(_) => docvec!["// TODO: Implement use statements"],
        }
    }

    fn expression(&self, expr: &TypedExpr) -> Document<'_> {
        match expr {
            TypedExpr::Int { value, .. } => docvec![value.to_string()],
            TypedExpr::Float { value, .. } => docvec![value.to_string()],
            TypedExpr::String { value, .. } => docvec!["\"", value, "\""],
            TypedExpr::Var { name, .. } => docvec![name],
            // Implement other expression types as needed
            _ => docvec!["// TODO: Implement other expression types"],
        }
    }

    fn assignment(&self, assignment: &TypedAssignment) -> Document<'_> {
        let name = self.pattern(&assignment.pattern);
        let value = self.expression(&assignment.value);
        docvec!["let ", name, " = ", value]
    }

    fn pattern(&self, pattern: &Pattern<Arc<Type>>) -> Document<'_> {
        match pattern {
            Pattern::Variable {
                location,
                name,
                type_,
            } => {
                docvec![name]
            }

            Pattern::Discard {
                name,
                location,
                type_,
            } => {
                docvec![name]
            }

            Pattern::List {
                location,
                elements,
                tail,
                type_,
            } => {
                docvec![
                    "[",
                    elements
                        .iter()
                        .map(|e| self.pattern(e))
                        .intersperse("; ".to_doc())
                        .collect::<Vec<_>>()
                        .to_doc(),
                    "]"
                ]
            }
            Pattern::Tuple { elems, .. } => {
                docvec![
                    "(",
                    elems
                        .iter()
                        .map(|e| self.pattern(e))
                        .intersperse("; ".to_doc())
                        .collect::<Vec<_>>()
                        .to_doc(),
                    ")"
                ]
            }
            _ => docvec!["// TODO: Implement other pattern types"],
        }
    }

    fn type_to_fsharp(&self, t: &Type) -> Document<'_> {
        match t {
            Type::Named { name, .. } => docvec![name],
            Type::Fn { args, retrn, .. } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.type_to_fsharp(arg))
                    .collect::<Vec<_>>();
                let return_type = self.type_to_fsharp(retrn);
                docvec![
                    arg_types
                        .iter()
                        .intersperse(&" -> ".to_doc())
                        .collect::<Vec<_>>(),
                    " -> ",
                    return_type
                ]
            }
            // Implement other type conversions as needed
            _ => docvec!["// TODO: Implement other type conversions"],
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        modules: &[Module],
    ) -> Result<()> {
        let project_file_path = self.build_dir.join(format!("{}.fsproj", &config.name));

        // Create project file content
        let project_file_content = format!(
            r#"<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net6.0</TargetFramework>
    <RootNamespace>{}</RootNamespace>
  </PropertyGroup>

  <ItemGroup>
{}
  </ItemGroup>

  <ItemGroup>
{}
  </ItemGroup>
</Project>"#,
            config.name,
            modules
                .iter()
                .map(|m| format!(
                    "    <Compile Include=\"{}.fs\" />",
                    m.name.replace("/", "\\")
                ))
                .collect::<Vec<_>>()
                .join("\n"),
            config
                .dependencies
                .iter()
                .map(|(name, version)| format!(
                    "    <PackageReference Include=\"{}\" Version=\"{}\" />",
                    name, version
                ))
                .collect::<Vec<_>>()
                .join("\n")
        );

        // Write project file
        writer.write(&project_file_path, &project_file_content)?;

        // Write individual module files
        for module in modules {
            let module_file_path = self
                .build_dir
                .join(format!("{}.fs", module.name.replace("/", "\\")));
            let module_content = self.module(module).to_string();
            writer.write(&module_file_path, &module_content)?;
        }

        Ok(())
    }
}
