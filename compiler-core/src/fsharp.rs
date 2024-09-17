#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    docvec,
    pretty::*,
    type_::{Type, TypeVar},
};
use ecow::EcoString;
use std::{ops::Deref, sync::Arc};
use vec1::Vec1;

#[derive(Debug, Clone)]
pub struct FSharp<'module> {
    module_name: &'module EcoString,
}

const INDENT: isize = 4;

impl<'module> FSharp<'module> {
    pub fn new(module_name: &'module EcoString) -> Self {
        Self { module_name }
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

        docs.extend(self.module_constants(module));

        join(docs, line())
    }

    fn module_declaration(&self, module: &TypedModule) -> Document<'module> {
        // Use module rec so we don't hav to worry about order
        docvec!["module rec ", module.name.replace("/", ".")]
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
                    docvec![c_name, " of ", join(fields, " * ".to_doc())]
                }
            })
            .collect::<Vec<Document<'module>>>();

        docvec![
            "type ",
            name,
            " =",
            line(),
            join(
                constructors.into_iter().map(|c| docvec!["| ".to_doc(), c]),
                line()
            )
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

    fn module_constants(&self, module: &'module TypedModule) -> Vec<Document<'module>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::ModuleConstant(c) => Some(self.module_constant(c)),
                _ => None,
            })
            .collect()
    }

    fn function_definitions(&self, module: &'module TypedModule) -> Vec<Document<'module>> {
        module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::Function(f) => {
                    let name = f.name.as_ref().map(|n| n.1.as_str()).unwrap_or("_");
                    Some(self.function(name, &f.arguments, &f.body, &f.return_type))
                }
                _ => None,
            })
            .collect()
    }

    fn function(
        &self,
        name: &'module str,
        arguments: &[TypedArg],
        body: &Vec1<TypedStatement>,
        return_type: &Type,
    ) -> Document<'module> {
        let args = if arguments.is_empty() {
            "()".to_doc()
        } else {
            self.fun_args(arguments)
        };

        let body = self.statements(body, Some(return_type));
        let return_type: Document<'module> = self.type_to_fsharp(return_type);

        "let "
            .to_doc()
            .append(name)
            .append("")
            .append(args.group())
            .append(": ")
            .append(return_type)
            .append(" = begin")
            .append(line().append(body).nest(INDENT).group())
            .append(line().append("end"))
    }

    fn fun_args(&self, arguments: &[TypedArg]) -> Document<'module> {
        join(
            arguments.iter().map(|arg| {
                "(".to_doc()
                    .append(docvec![
                        arg.names
                            .get_variable_name()
                            .map(|n| n.to_doc())
                            .unwrap_or_else(|| "_".to_doc()),
                        ": ",
                        self.type_to_fsharp(&arg.type_)
                    ])
                    .append(")")
            }),
            " ".to_doc(),
        )
    }

    // Anon
    fn fun(&self, args: &[TypedArg], body: &Vec1<TypedStatement>) -> Document<'module> {
        "fun"
            .to_doc()
            .append(self.fun_args(args).append(" ->"))
            .append(self.statements(body, None).nest(INDENT))
            .append(break_("", " "))
            .group()
    }

    fn statements(
        &self,
        statements: &Vec1<TypedStatement>,
        return_type: Option<&Type>,
    ) -> Document<'module> {
        let mut last_var = None;
        let mut res = statements
            .iter()
            .map(|s| match s {
                Statement::Expression(expr) => {
                    last_var = None;
                    self.expression(expr)
                }
                Statement::Assignment(assignment) => {
                    let (name, value) = self.get_assignment_info(assignment);
                    last_var = Some(name.clone());
                    "let ".to_doc().append(name).append(" = ").append(value)
                }
                Statement::Use(_) => docvec!["// TODO: Implement use statements"],
            })
            .collect::<Vec<Document<'module>>>();

        // Can't end on an assignment in F# unless it returns Unit

        if let Some(last_var) = last_var {
            match return_type {
                Some(return_type) => {
                    if !return_type.is_nil() {
                        res.push(last_var);
                    }
                }
                None => {
                    res.push(last_var);
                }
            }
        }

        join(res, line()).group()
    }

    fn expression(&self, expr: &TypedExpr) -> Document<'module> {
        match expr {
            TypedExpr::Int { value, .. } => value.to_doc(),
            TypedExpr::Float { value, .. } => value.to_doc(),
            TypedExpr::String { value, .. } => docvec!["\"", value, "\""],
            TypedExpr::Block { statements, .. } => self.block(statements),
            TypedExpr::Pipeline {
                assignments,
                finally,
                ..
            } => self.pipeline(assignments, finally),
            TypedExpr::Var { name, .. } => docvec![name],
            TypedExpr::Fn { args, body, .. } => self.fun(args, body),
            TypedExpr::List { elements, .. } => {
                docvec![
                    "[",
                    join(elements.iter().map(|e| self.expression(e)), "; ".to_doc()),
                    "]"
                ]
            }
            TypedExpr::Call { fun, args, .. } => self
                .expression(fun)
                .append(" ")
                .append(join(args.iter().map(|a| self.expression(&a.value)), " ".to_doc()).group()),

            TypedExpr::BinOp {
                left, right, name, ..
            } => self.binop(name, left, right),

            TypedExpr::Case {
                subjects, clauses, ..
            } => "// Pattern matching not yet implemented".to_doc(),

            _ => docvec!["// TODO: Implement other expression types"],
        }
    }

    fn tuple(&self, elements: impl IntoIterator<Item = Document<'module>>) -> Document<'module> {
        docvec!["(", join(elements, ", ".to_doc()), ")"]
    }

    // fn case(&self, subjects: &Vec<TypedExpr>, clauses: &Vec<TypedClause>) -> Document<'module> {
    //     let subjects_doc = if subjects.len() == 1 {
    //         self.expression(
    //             subjects
    //                 .first()
    //                 .expect("f# case printing of single subject"),
    //         )
    //     } else {
    //         self.tuple(subjects.iter().map(|s| self.expression(s)))
    //     };

    //     let mut res = "match "
    //         .to_doc()
    //         .append(subjects_doc)
    //         .append(" with")
    //         .append(self.clauses(clauses))
    //         .group();

    //     res
    // }

    // fn clauses(&self, clauses: &Vec<TypedClause>) -> Document<'module> {
    //     join(
    //         clauses
    //             .iter()
    //             .map(|clause| "| ".to_doc().append(self.clause(clause))),
    //         line(),
    //     )
    // }

    // fn clause(&self, clause: &TypedClause) -> Document<'module> {
    //     let Clause {
    //         guard,
    //         pattern: pat,
    //         alternative_patterns,
    //         then,
    //         ..
    //     } = clause;

    //     join(
    //         std::iter::once(pat)
    //             .chain(alternative_patterns)
    //             .map(|patterns| {
    //                 let patterns_doc = if patterns.len() == 1 {
    //                     let p = patterns.first().expect("Single pattern clause printing");
    //                     self.pattern(p)
    //                 } else {
    //                     self.tuple(patterns.iter().map(|p| self.pattern(p)))
    //                 };

    //                 let guard = optional_clause_guard(guard.as_ref());
    //                 patterns_doc.append(
    //                     guard
    //                         .append(" ->")
    //                         .append(line().append(then_doc.clone()).nest(INDENT).group()),
    //                 )
    //             }),
    //         "|".to_doc(),
    //     )
    // }

    // fn optional_clause_guard<'a>(guard: Option<&'a TypedClauseGuard>) -> Document<'a> {
    //     let guard_doc = guard.map(|guard| bare_clause_guard(guard, env));

    //     let guards_count = guard_doc.iter().len() + additional_guards.len();
    //     let guards_docs = additional_guards.into_iter().chain(guard_doc).map(|guard| {
    //         if guards_count > 1 {
    //             guard.surround("(", ")")
    //         } else {
    //             guard
    //         }
    //     });
    //     let doc = join(guards_docs, " andalso ".to_doc());
    //     if doc.is_empty() {
    //         doc
    //     } else {
    //         " when ".to_doc().append(doc)
    //     }
    // }
    fn binop(&self, name: &BinOp, left: &TypedExpr, right: &TypedExpr) -> Document<'module> {
        let operand = match name {
            // Boolean logic
            BinOp::And => "&&",
            BinOp::Or => "||",

            // Equality
            BinOp::Eq => "=",
            BinOp::NotEq => "<>",

            // Order comparison
            BinOp::LtInt | BinOp::LtFloat => "<",
            BinOp::LtEqInt | BinOp::LtEqFloat => "<=",
            BinOp::GtInt | BinOp::GtFloat => ">",
            BinOp::GtEqInt | BinOp::GtEqFloat => ">=",

            // Maths
            BinOp::AddInt | BinOp::AddFloat => "+",
            BinOp::SubInt | BinOp::SubFloat => "-",
            BinOp::MultInt | BinOp::MultFloat => "*",
            BinOp::DivInt | BinOp::DivFloat => "/",
            BinOp::RemainderInt => "%",

            // Strings
            BinOp::Concatenate => "+",
        };
        self.expression(left)
            .append(" ")
            .append(operand)
            .append(" ")
            .append(self.expression(right))
    }

    fn pipeline(
        &self,
        assignments: &Vec<TypedAssignment>,
        finally: &TypedExpr,
    ) -> Document<'module> {
        "//TODO: implement pipelines".to_doc()
    }

    fn block(&self, statements: &Vec1<TypedStatement>) -> Document<'module> {
        "do ".to_doc().append(line()).nest(INDENT).append(
            self.statements(statements, None)
                .append(line().append("()"))
                .nest(INDENT)
                .group(),
        )
    }

    fn get_assignment_info(
        &self,
        assignment: &TypedAssignment,
    ) -> (Document<'module>, Document<'module>) {
        let name = self.pattern(&assignment.pattern);
        let value = self.expression(&assignment.value);
        (name, value)
    }

    fn pattern(&self, pattern: &Pattern<Arc<Type>>) -> Document<'module> {
        match pattern {
            Pattern::Variable { name, .. } => name.to_doc(),
            Pattern::Discard { name, .. } => name.to_doc(),
            Pattern::List { elements, .. } => {
                docvec![
                    "[",
                    join(elements.iter().map(|e| self.pattern(e)), "; ".to_doc()),
                    "]"
                ]
            }
            Pattern::Tuple { elems, .. } => {
                docvec![
                    "(",
                    join(elems.iter().map(|e| self.pattern(e)), ", ".to_doc()),
                    ")"
                ]
            }
            _ => docvec!["// TODO: Implement other pattern types"],
        }
    }

    fn type_to_fsharp(&self, t: &Type) -> Document<'module> {
        if t.is_nil() {
            return "unit".to_doc();
        }

        match t {
            Type::Named { name, .. } => match name.as_str() {
                "Int" | "int" => "int".to_doc(),
                "Float" | "float" => "float64".to_doc(),
                "String" | "string" => "string".to_doc(),
                "Bool" | "bool" => "bool".to_doc(),
                _ => name.to_doc(),
            },
            Type::Fn { args, retrn, .. } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.type_to_fsharp(arg))
                    .collect::<Vec<Document<'module>>>();
                let return_type = self.type_to_fsharp(retrn);
                docvec![
                    join(arg_types.into_iter(), " -> ".to_doc()),
                    " -> ",
                    return_type
                ]
            }
            Type::Tuple { elems } => {
                docvec![
                    "(",
                    join(elems.iter().map(|e| self.type_to_fsharp(e)), "; ".to_doc()),
                    ")"
                ]
            }
            Type::Var { type_ } => {
                let type_ = type_.borrow();
                match type_.deref() {
                    TypeVar::Link { type_ } => self.type_to_fsharp(type_),
                    TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                        docvec!["var_", id]
                    }
                }
            }
        }
    }

    fn module_constant(
        &self,
        constant: &'module ModuleConstant<Arc<Type>, EcoString>,
    ) -> Document<'module> {
        let name = constant.name.as_str();
        let value = &constant.value;

        match value.deref().clone() {
            Constant::Int { value, .. }
            | Constant::Float { value, .. }
            | Constant::String { value, .. } => {
                docvec![
                    "[<Literal>]",
                    line(),
                    "let ",
                    name.to_doc(),
                    " = ",
                    value.to_doc(),
                ]
            }
            _ => docvec![
                "let ",
                name.to_doc(),
                " = ",
                "// TODO: Return Result instead here"
            ],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
}

pub fn render_module(module: &TypedModule) -> super::Result<String> {
    let document = FSharp::new(&module.name).module(module);

    Ok(document.to_pretty_string(120))
}
