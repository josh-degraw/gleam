#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    docvec,
    pretty::*,
    type_::{Type, TypeVar},
};
use ecow::EcoString;
use regex::{Captures, Regex};
use std::{
    ops::Deref,
    sync::{Arc, OnceLock},
};

const INDENT: isize = 4;

pub fn render_module(module: &TypedModule) -> super::Result<String> {
    let document = module_to_doc(module);
    Ok(document.to_pretty_string(120))
}

fn module_to_doc(module: &TypedModule) -> Document<'_> {
    join(vec![module_declaration(module), contents(module)], line())
}

fn module_declaration(module: &TypedModule) -> Document<'_> {
    "module rec ".to_doc().append(santitize_name(&module.name))
}

fn santitize_name(name: &EcoString) -> Document<'_> {
    join(
        name.split("/").map(|s| {
            if is_reserved_word(s) {
                "``".to_doc().append(s.to_doc()).append("``")
            } else {
                s.to_doc()
            }
        }),
        ".".to_doc(),
    )
}

fn is_reserved_word(name: &str) -> bool {
    matches!(
        name,
        "asr"
            | "land"
            | "lor"
            | "lsl"
            | "lsr"
            | "lxor"
            | "mod"
            | "sig"
            | "break"
            | "checked"
            | "component"
            | "const"
            | "constraint"
            | "continue"
            | "event"
            | "external"
            | "include"
            | "mixin"
            | "parallel"
            | "process"
            | "protected"
            | "pure"
            | "sealed"
            | "tailcall"
            | "trait"
            | "virtual"
    )
}

fn contents<'a>(module: &'a TypedModule) -> Document<'a> {
    join(
        module
            .definitions
            .iter()
            .map(|def| match def {
                Definition::CustomType(t) => custom_type(t),
                Definition::TypeAlias(t) => type_alias(t),
                Definition::ModuleConstant(c) => module_constant(c),
                Definition::Function(f) => {
                    let name = f.name.as_ref().map(|n| n.1.as_str()).unwrap_or("_");
                    function(name, &f.arguments, &f.body, &f.return_type)
                }
                Definition::Import(_) => docvec!["// TODO: Implement imports"],
            })
            .collect::<Vec<Document<'a>>>(),
        line(),
    )
}

fn custom_type<'a>(t: &'a CustomType<Arc<Type>>) -> Document<'a> {
    let name = &t.name;
    let constructors = t
        .constructors
        .iter()
        .map(|c| {
            let fields = c
                .arguments
                .iter()
                .map(|f| type_to_fsharp(&f.type_))
                .collect::<Vec<Document<'a>>>();

            let c_name = c.name.clone().to_doc();
            if fields.is_empty() {
                c_name
            } else {
                docvec![c_name, " of ", join(fields, " * ".to_doc())]
            }
        })
        .collect::<Vec<Document<'a>>>();

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

fn type_alias(t: &TypeAlias<Arc<Type>>) -> Document<'_> {
    docvec!["type ", t.alias.clone(), " = ", type_to_fsharp(&t.type_)]
}

fn function<'a>(
    name: &'a str,
    arguments: &'a [TypedArg],
    body: &'a [TypedStatement],
    return_type: &'a Type,
) -> Document<'a> {
    let args = if arguments.is_empty() {
        "()".to_doc()
    } else {
        fun_args(arguments)
    };

    let body = statements(body, Some(return_type));
    let return_type = type_to_fsharp(return_type);

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
fn fun_args(arguments: &[TypedArg]) -> Document<'_> {
    join(
        arguments.iter().map(|arg| {
            "(".to_doc()
                .append(docvec![
                    arg.names
                        .get_variable_name()
                        .map(|n| n.to_doc())
                        .unwrap_or_else(|| "_".to_doc()),
                    ": ",
                    type_to_fsharp(&arg.type_)
                ])
                .append(")")
        }),
        " ".to_doc(),
    )
}

/// Anonymous functions
fn fun<'a>(args: &'a [TypedArg], body: &'a [TypedStatement]) -> Document<'a> {
    "fun"
        .to_doc()
        .append(fun_args(args).append(" -> "))
        .append(statements(body, None).nest(INDENT))
        .append(break_("", " "))
        .group()
}

fn statements<'a>(s: &'a [TypedStatement], return_type: Option<&Type>) -> Document<'a> {
    let mut last_var = None;
    let mut res = s
        .iter()
        .map(|s| match s {
            Statement::Expression(expr) => {
                last_var = None;
                expression(expr)
            }
            Statement::Assignment(assignment) => {
                let (name, value) = get_assignment_info(assignment);
                last_var = Some(name.clone());
                "let ".to_doc().append(name).append(" = ").append(value)
            }
            Statement::Use(_) => docvec!["// TODO: Implement use statements"],
        })
        .collect::<Vec<Document<'a>>>();

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

fn unicode_escape_sequence_pattern() -> &'static Regex {
    static PATTERN: OnceLock<Regex> = OnceLock::new();
    PATTERN.get_or_init(|| {
        Regex::new(r#"(\\+)(u)\{(.+)\}"#)
            .expect("Unicode escape sequence regex cannot be constructed")
    })
}

fn string_inner<'a>(value: &str) -> Document<'a> {
    let content = unicode_escape_sequence_pattern()
        // `\\u`-s should not be affected, so that "\\u..." is not converted to
        // "\\u...". That's why capturing groups is used to exclude cases that
        // shouldn't be replaced.
        .replace_all(value, |caps: &Captures<'_>| {
            let slashes = caps.get(1).map_or("", |m| m.as_str());
            let unicode = caps.get(3).map_or("", |m| m.as_str());

            println!("slashes: {}", slashes);
            if slashes.len() % 2 == 0 {
                // TODO: See if we can emit a warning here because it probably wasn't intentional
                format!("{slashes}u{{{unicode}}}") // return the original string
            } else {
                format!("{slashes}u{unicode}")
            }
        })
        .to_string();
    Document::String(content)
}

fn string<'a>(value: &str) -> Document<'a> {
    string_inner(value).surround("\"", "\"")
}

fn expression(expr: &TypedExpr) -> Document<'_> {
    match expr {
        TypedExpr::Int { value, .. } => value.to_doc(),
        TypedExpr::Float { value, .. } => value.to_doc(),
        TypedExpr::String { value, .. } => string(value.as_str()),
        TypedExpr::Block { statements, .. } => block(statements),
        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => pipeline(assignments, finally),
        TypedExpr::Var { name, .. } => match name.as_str() {
            "Nil" => "()".to_doc(),
            _ => name.to_doc(),
        },
        TypedExpr::Fn { args, body, .. } => fun(args, body),
        TypedExpr::List { elements, .. } => {
            join(elements.iter().map(expression), "; ".to_doc()).surround("[", "]")
        }
        TypedExpr::Call { fun, args, .. } => {
            let args = if args.is_empty() {
                "()".to_doc()
            } else {
                " ".to_doc().append(
                    join(
                        args.iter().map(|a| expression(&a.value).surround("(", ")")),
                        " ".to_doc(),
                    )
                    .group(),
                )
            };
            let fun_expr = expression(fun);
            // If for some reason we're doing an IIFE, we need to wrap it in parens
            let fun_expr = match fun.as_ref() {
                TypedExpr::Fn { .. } => fun_expr.surround("(", ")"),
                _ => fun_expr,
            };
            fun_expr.append(args).group()
        }

        TypedExpr::BinOp {
            left, right, name, ..
        } => binop(name, left, right),

        TypedExpr::Case {
            subjects, clauses, ..
        } => "// Pattern matching not yet implemented".to_doc(),

        TypedExpr::Tuple { elems, .. } => tuple(elems),
        TypedExpr::NegateInt { value, .. } => "-".to_doc().append(expression(value)),

        TypedExpr::Todo { message, .. } => todo(message),
        TypedExpr::Panic { message, .. } => panic_(message),

        _ => docvec!["// TODO: Implement other expression types"],
    }
}

fn todo(message: &Option<Box<TypedExpr>>) -> Document<'_> {
    match message {
        Some(message) => "failwith "
            .to_doc()
            .append(expression(message.as_ref()).surround("(", ")")),
        None => "failwith \"Not implemented\"".to_doc(),
    }
}

fn panic_(message: &Option<Box<TypedExpr>>) -> Document<'_> {
    match message {
        Some(message) => "failwith "
            .to_doc()
            .append(expression(message.as_ref()).surround("(", ")")),
        None => "failwith \"Panic encountered\"".to_doc(),
    }
}

fn tuple(elements: &[TypedExpr]) -> Document<'_> {
    join(elements.iter().map(expression), ", ".to_doc()).surround("(", ")")
}
// fn case(&self, subjects: &Vec<TypedExpr>, clauses: &Vec<TypedClause>) -> Document<'a> {
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

// fn clauses(&self, clauses: &Vec<TypedClause>) -> Document<'a> {
//     join(
//         clauses
//             .iter()
//             .map(|clause| "| ".to_doc().append(self.clause(clause))),
//         line(),
//     )
// }

// fn clause(&self, clause: &TypedClause) -> Document<'a> {
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
fn binop<'a>(name: &BinOp, left: &'a TypedExpr, right: &'a TypedExpr) -> Document<'a> {
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
    expression(left)
        .append(" ")
        .append(operand)
        .append(" ")
        .append(expression(right))
}

// Implement pipeline (|>) expressions

fn pipeline<'a>(assignments: &'a [TypedAssignment], finally: &'a TypedExpr) -> Document<'a> {
    let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

    for a in assignments {
        let (name, value) = get_assignment_info(a);

        documents.push("let ".to_doc().append(name).append(" = ").append(value));
        documents.push(line());
    }

    documents.push(expression(finally));
    documents.to_doc()
}

/// Given a block like this:
/// If given a block that ends in an assignment, we need to return the value of the last assignment
///
fn block<'a>(s: &'a [TypedStatement]) -> Document<'a> {
    "begin"
        .to_doc()
        .append(line())
        .nest(INDENT)
        .append(statements(s, None).nest(INDENT).group())
        .append(line().append("end"))
    // if s.len() == 1 {
    //     let statement = s.first().expect("single-line block statement");
    //     match statement {
    //         TypedStatement::Expression(expr) => expression(expr),
    //         Statement::Assignment(assignment) => {
    //             let (name, value) = get_assignment_info(assignment);
    //             "let "
    //                 .to_doc()
    //                 .append(name)
    //                 .append(" = ")
    //                 .append(value.clone().to_doc())
    //                 .append(line())
    //                 .append(value.to_doc())
    //         }
    //         _ => docvec!["// TODO: Implement other statement types"],
    //     }
    // } else {
    //     // To ensure scoping remains valid, if the return type is Nil, we need to
    //     // wrap the statements in a do block so that the result is discarded
    //     let final_statement = s.last().expect("final type");
    //     if final_statement.type_().is_nil() {
    //         "do ".to_doc().append(line()).nest(INDENT).append(
    //             statements(s, None)
    //                 .append(line().append("()"))
    //                 .nest(INDENT)
    //                 .group(),
    //         )
    //     } else {
    //         // Otherwise we should treat it like a normal sequence of statments
    //         // TODO: Make sure this works if the final statement is an assignment

    //         "let _ = ".to_doc().append(line()).nest(INDENT).append(
    //             statements(s, Some(&final_statement.type_()))
    //                 .nest(INDENT)
    //                 .group(),
    //         )
    //     }
    //}
}

fn get_assignment_info(assignment: &TypedAssignment) -> (Document<'_>, Document<'_>) {
    let name = pattern(&assignment.pattern);
    let value = expression(&assignment.value);
    (name, value)
}

fn pattern(p: &Pattern<Arc<Type>>) -> Document<'_> {
    match p {
        Pattern::Variable { name, .. } => name.to_doc(),
        Pattern::Discard { name, .. } => name.to_doc(),
        Pattern::List { elements, .. } => {
            join(elements.iter().map(pattern), "; ".to_doc()).surround("[", "]")
        }
        Pattern::Tuple { elems, .. } => {
            join(elems.iter().map(pattern), ", ".to_doc()).surround("(", ")")
        }
        _ => docvec!["// TODO: Implement other pattern types"],
    }
}

fn type_to_fsharp<'a>(t: &Type) -> Document<'a> {
    if t.is_nil() {
        return "unit".to_doc();
    }

    match t {
        Type::Named { name, .. } => match name.as_str() {
            "Int" | "int" => "int".to_doc(),
            "Float" | "float" => "float".to_doc(),
            "String" | "string" => "string".to_doc(),
            "Bool" | "bool" => "bool".to_doc(),
            _ => name.to_doc(),
        },
        Type::Fn { args, retrn, .. } => {
            let arg_types = args
                .iter()
                .map(|arg| type_to_fsharp(arg))
                .collect::<Vec<Document<'a>>>();

            let arg_types = if arg_types.is_empty() {
                "unit".to_doc()
            } else {
                join(arg_types, " -> ".to_doc())
            };

            let return_type = type_to_fsharp(retrn);
            docvec![arg_types, " -> ", return_type]
        }
        Type::Tuple { elems } => {
            join(elems.iter().map(|t| type_to_fsharp(t)), "; ".to_doc()).surround("(", ")")
        }
        Type::Var { type_ } => {
            let borrowed = type_.borrow();
            match borrowed.deref() {
                TypeVar::Link { type_ } => type_to_fsharp(type_),
                TypeVar::Unbound { id } => Document::String(format!("'u{}", id)),
                TypeVar::Generic { id } => Document::String(format!("'t{}", id)),
            }
        }
    }
}

fn module_constant(constant: &ModuleConstant<Arc<Type>, EcoString>) -> Document<'_> {
    let name = constant.name.as_str();
    let value = &constant.value;

    let binding = "[<Literal>]"
        .to_doc()
        .append(line())
        .append("let ")
        .append(name.to_doc())
        .append(" = ");
    match value.deref().clone() {
        Constant::Int { value, .. } | Constant::Float { value, .. } => {
            binding.append(value.to_doc())
        }
        Constant::String { value, .. } => binding.append(string(value.as_str())),
        _ => binding
            .append("// TODO: haven't figured out how to handle this constant type yet".to_doc()),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
}
