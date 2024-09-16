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

    pub fn module(&self, module: &TypedModule) -> Document<'a> {
        let mut docs = Vec::new();

        // Add module declaration
        docs.push(self.module_declaration(module));

        // Add imports
        // docs.extend(self.imports(module));

        // Add type definitions
        docs.extend(self.type_definitions(module));

        // Add function definitions
        docs.extend(self.function_definitions(module));

        docvec![Itertools::intersperse(docs.into_iter(), line()).collect::<Vec<Document<'a>>>()]
    }

    fn module_declaration(&self, module: &TypedModule) -> Document<'a> {
        docvec!["module ", module.name.replace("/", ".")]
    }

    // fn imports(&self, module: &TypedModule) -> Vec<Document<'a>> {
    //     module
    //         .
    //         .iter()
    //         .map(|import| {
    //             let module_name = import.module.replace("/", ".");
    //             docvec!["open ", module_name]
    //         })
    //         .collect()
    // }

    fn type_definitions(&self, module: &TypedModule) -> Vec<Document<'a>> {
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

    fn custom_type(&self, t: &CustomType<Arc<Type>>) -> Document<'a> {
        let name = &t.name;
        let constructors = t
            .constructors
            .iter()
            .map(|c| {
                let fields = c
                    .arguments
                    .iter()
                    .map(|f| self.type_to_fsharp(&f.type_))
                    .collect::<Vec<Document<'a>>>();

                let c_name = c.name.clone().to_doc();
                if fields.is_empty() {
                    c_name
                } else {
                    docvec![
                        c_name,
                        " of ",
                        Itertools::intersperse(fields.into_iter(), " * ".to_doc())
                            .collect::<Vec<Document<'a>>>()
                    ]
                }
            })
            .collect::<Vec<Document<'a>>>();

        docvec![
            "type ",
            name,
            " =",
            line(),
            Itertools::intersperse(
                constructors.into_iter().map(|c| docvec!["| ".to_doc(), c]),
                line()
            )
            .collect::<Vec<Document<'a>>>()
        ]
    }

    fn type_alias(&self, t: &TypeAlias<Arc<Type>>) -> Document<'a> {
        docvec![
            "type ",
            t.alias.clone(),
            " = ",
            self.type_to_fsharp(&t.type_)
        ]
    }

    fn function_definitions(&self, module: &TypedModule) -> Vec<Document<'a>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::Function(f) => Some(self.function(f)),
                _ => None,
            })
            .collect()
    }

    fn function(&self, f: &Function<Arc<Type>, TypedExpr>) -> Document<'a> {
        let name = &f.name;
        let name = name
            .as_ref()
            .map(|n| n.1.as_ref().to_doc())
            .unwrap_or_else(|| "_".to_doc());
        let args = f
            .arguments
            .into_iter()
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
            .collect::<Vec<Document<'a>>>();
        let return_type: Document<'a> = self.type_to_fsharp(&f.return_type);
        let body: Vec<Document<'a>> = f.body.iter().map(|b| self.statement(&b)).collect();

        let args_doc =
            Document::Vec(Itertools::intersperse(args.into_iter(), " ".to_doc()).collect());

        docvec![
            "let ",
            name,
            " ",
            args_doc,
            ": ",
            return_type,
            " =",
            line(),
            body
        ]
    }

    fn statements(&self, statements: &Vec1<Statement<Arc<Type>, TypedExpr>>) -> Document<'a> {
        statements
            .iter()
            .map(|s| self.statement(s))
            .collect::<Vec<Document<'a>>>()
            .to_doc()
    }

    fn statement(&self, statement: &Statement<Arc<Type>, TypedExpr>) -> Document<'a> {
        match statement {
            Statement::Expression(expr) => self.expression(expr),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(_) => docvec!["// TODO: Implement use statements"],
        }
    }

    fn expression(&self, expr: &TypedExpr) -> Document<'a> {
        match expr {
            TypedExpr::Int { value, .. } => value.to_doc(),
            TypedExpr::Float { value, .. } => value.to_doc(),
            TypedExpr::String { value, .. } => docvec!["\"", value, "\""],
            TypedExpr::Var { name, .. } => docvec![name],
            // Implement other expression types as needed
            _ => docvec!["// TODO: Implement other expression types"],
        }
    }

    fn assignment(&self, assignment: &TypedAssignment) -> Document<'a> {
        let name = self.pattern(&assignment.pattern);
        let value = self.expression(&assignment.value);
        docvec!["let ", name, " = ", value]
    }

    fn pattern(&self, pattern: &Pattern<Arc<Type>>) -> Document<'a> {
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
                        .collect::<Vec<Document<'a>>>()
                        .to_doc(),
                    "]"
                ]
            }
            Pattern::Tuple { elems, .. } => {
                docvec![
                    "(",
                    Itertools::intersperse(elems.iter().map(|e| self.pattern(e)), "; ".to_doc())
                        .collect::<Vec<Document<'a>>>()
                        .to_doc(),
                    ")"
                ]
            }
            _ => docvec!["// TODO: Implement other pattern types"],
        }
    }

    fn type_to_fsharp(&self, t: &Type) -> Document<'a> {
        match t {
            Type::Named { name, .. } => docvec![name],
            Type::Fn { args, retrn, .. } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.type_to_fsharp(arg))
                    .collect::<Vec<Document<'a>>>();
                let return_type = self.type_to_fsharp(retrn);
                docvec![
                    Itertools::intersperse(arg_types.into_iter(), " -> ".to_doc())
                        .collect::<Vec<Document<'a>>>(),
                    " -> ",
                    return_type
                ]
            }
            // Implement other type conversions as needed
            _ => docvec!["// TODO: Implement other type conversions"],
        }
    }
}
