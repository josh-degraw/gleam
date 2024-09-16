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

pub struct FSharp<'module> {
    build_dir: &'module camino::Utf8Path,
}

impl<'module> FSharp<'module> {
    pub fn new(build_dir: &'module camino::Utf8Path) -> Self {
        Self { build_dir }
    }

    pub fn module(&self, module: &'module TypedModule) -> Document<'module> {
        let mut docs = Vec::new();

        // Add module declaration
        docs.push(self.module_declaration(module));

        // TODO: Add imports
        // docs.extend(self.imports(module));

        // Add type definitions
        docs.extend(self.type_definitions(module));

        // Add function definitions
        docs.extend(self.function_definitions(module));

        docvec![Itertools::intersperse(docs.into_iter(), line()).collect::<Vec<Document<'module>>>()]
    }

    fn module_declaration(&self, module: &TypedModule) -> Document<'module> {
        docvec!["module ", module.name.replace("/", ".")]
    }

    // fn imports(&self, module: &TypedModule) -> Vec<Document<'module>> {
    //     module
    //         .
    //         .iter()
    //         .map(|import| {
    //             let module_name = import.module.replace("/", ".");
    //             docvec!["open ", module_name]
    //         })
    //         .collect()
    // }

    fn type_definitions(&self, module: &TypedModule) -> Vec<Document<'module>> {
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

    fn custom_type(&self, t: &CustomType<Arc<Type>>) -> Document<'module> {
        let name = &t.name;
        let constructors = t
            .constructors
            .iter()
            .map(|c| {
                let fields = c
                    .arguments
                    .iter()
                    .map(|f| self.type_to_fsharp(&f.type_))
                    .collect::<Vec<Document<'module>>>();

                let c_name = c.name.clone().to_doc();
                if fields.is_empty() {
                    c_name
                } else {
                    docvec![
                        c_name,
                        " of ",
                        Itertools::intersperse(fields.into_iter(), " * ".to_doc())
                            .collect::<Vec<Document<'module>>>()
                    ]
                }
            })
            .collect::<Vec<Document<'module>>>();

        docvec![
            "type ",
            name,
            " =",
            line(),
            Itertools::intersperse(
                constructors.into_iter().map(|c| docvec!["| ".to_doc(), c]),
                line()
            )
            .collect::<Vec<Document<'module>>>()
        ]
    }

    fn type_alias(&self, t: &TypeAlias<Arc<Type>>) -> Document<'module> {
        docvec![
            "type ",
            t.alias.clone(),
            " = ",
            self.type_to_fsharp(&t.type_)
        ]
    }

    fn function_definitions(&self, module: &'module TypedModule) -> Vec<Document<'module>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::Function(f) => Some(self.function(f)),
                _ => None,
            })
            .collect()
    }

    fn function(&self, f: &'module Function<Arc<Type>, TypedExpr>) -> Document<'module> {
        let name = &f.name;
        let name = name
            .as_ref()
            .map(|n| n.1.as_ref().to_doc())
            .unwrap_or_else(|| "_".to_doc());
        let args = f
            .arguments
            .iter()
            .map(|arg| {
                docvec![
                    arg.names
                        .get_variable_name()
                        .map(|n| n.to_doc())
                        .unwrap_or_else(|| "_".to_doc()),
                    ": ",
                    self.type_to_fsharp(&arg.type_)
                ]
            })
            .collect::<Vec<Document<'module>>>();
        let return_type: Document<'module> = self.type_to_fsharp(&f.return_type);
        let body: Vec<Document<'module>> = f.body.iter().map(|b| self.statement(&b)).collect();

        let args_doc =
            Document::Vec(Itertools::intersperse(args.into_iter(), " ".to_doc()).collect());

        Document::Vec(vec![
            "let ".to_doc(),
            name,
            " ".to_doc(),
            args_doc,
            ": ".to_doc(),
            return_type,
            " =".to_doc(),
            line(),
            Document::Vec(body),
        ])
    }

    fn statements(&self, statements: &Vec1<Statement<Arc<Type>, TypedExpr>>) -> Document<'module> {
        statements
            .iter()
            .map(|s| self.statement(s))
            .collect::<Vec<Document<'module>>>()
            .to_doc()
    }

    fn statement(&self, statement: &Statement<Arc<Type>, TypedExpr>) -> Document<'module> {
        match statement {
            Statement::Expression(expr) => self.expression(expr),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(_) => docvec!["// TODO: Implement use statements"],
        }
    }

    fn expression(&self, expr: &TypedExpr) -> Document<'module> {
        match expr {
            TypedExpr::Int { value, .. } => value.to_doc(),
            TypedExpr::Float { value, .. } => value.to_doc(),
            TypedExpr::String { value, .. } => docvec!["\"", value, "\""],
            TypedExpr::Var { name, .. } => docvec![name],
            // Implement other expression types as needed
            _ => docvec!["// TODO: Implement other expression types"],
        }
    }

    fn assignment(&self, assignment: &TypedAssignment) -> Document<'module> {
        let name = self.pattern(&assignment.pattern);
        let value = self.expression(&assignment.value);
        docvec!["let ", name, " = ", value]
    }

    fn pattern(&self, pattern: &Pattern<Arc<Type>>) -> Document<'module> {
        match pattern {
            Pattern::Variable { name, .. } => {
                docvec![name]
            }

            Pattern::Discard { name, .. } => {
                docvec![name]
            }

            Pattern::List { elements, .. } => {
                docvec![
                    "[",
                    Itertools::intersperse(elements.iter().map(|e| self.pattern(e)), "; ".to_doc())
                        .collect::<Vec<Document<'module>>>()
                        .to_doc(),
                    "]"
                ]
            }
            Pattern::Tuple { elems, .. } => {
                docvec![
                    "(",
                    Itertools::intersperse(elems.iter().map(|e| self.pattern(e)), "; ".to_doc())
                        .collect::<Vec<Document<'module>>>()
                        .to_doc(),
                    ")"
                ]
            }
            _ => docvec!["// TODO: Implement other pattern types"],
        }
    }

    fn type_to_fsharp(&self, t: &Type) -> Document<'module> {
        match t {
            Type::Named { name, .. } => docvec![name],
            Type::Fn { args, retrn, .. } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.type_to_fsharp(arg))
                    .collect::<Vec<Document<'module>>>();
                let return_type = self.type_to_fsharp(retrn);
                docvec![
                    Itertools::intersperse(arg_types.into_iter(), " -> ".to_doc())
                        .collect::<Vec<Document<'module>>>(),
                    " -> ",
                    return_type
                ]
            }
            // Implement other type conversions as needed
            _ => docvec!["// TODO: Implement other type conversions"],
        }
    }
}
