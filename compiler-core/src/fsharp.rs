#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    docvec,
    pretty::*,
    type_::{Type, TypeVar, ValueConstructorVariant},
};
use ecow::EcoString;
use regex::{Captures, Regex};
use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, OnceLock},
};

const INDENT: isize = 4;
pub const FSHARP_PRELUDE: &str = include_str!("./fsharp/prelude.fs");

mod prelude_functions {
    pub const STRING_PATTERN_PREFIX: &str = "Gleam__codegen__prefix";
}

pub fn render_module(module: &TypedModule) -> super::Result<String> {
    let document = module_to_doc(module);
    Ok(document.to_pretty_string(120))
}

fn module_to_doc(module: &TypedModule) -> Document<'_> {
    join(
        vec![module_declaration(module), module_contents(module)],
        line(),
    )
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

fn module_contents<'a>(module: &'a TypedModule) -> Document<'a> {
    join(
        module
            .definitions
            .iter()
            .map(|def| match def {
                Definition::CustomType(t) if t.constructors.len() == 1 => record_type(t),
                Definition::CustomType(t) => discriminated_union(t),
                Definition::TypeAlias(t) => type_alias(t),
                Definition::ModuleConstant(c) => module_constant(c),
                Definition::Function(f) => function(f),
                Definition::Import(_) => docvec!["// TODO: Implement imports"],
            })
            .collect::<Vec<Document<'a>>>(),
        line(),
    )
}

fn map_publicity<'a>(publicity: Publicity) -> Document<'a> {
    match publicity {
        Publicity::Public => "".to_doc(),
        Publicity::Internal { .. } => "internal ".to_doc(),
        Publicity::Private => "private ".to_doc(),
    }
}

/// Should convert a single-constructor custom type to an F# record
/// gleam type:
///
/// ```gleam
/// pub type Person { Person(name: String, age: Int) }
/// ```
///
/// f# type:
///
/// ```fsharp
/// type Person = { name: string; age: int }
fn record_type<'a>(t: &'a CustomType<Arc<Type>>) -> Document<'a> {
    println!("record_type: {}", t.name);
    assert!(
        t.constructors.len() == 1,
        "Records must have a single constructor to count as a record"
    );

    let constructor = &t
        .constructors
        .first()
        .expect("Single constructor should exist");

    let name = &t.name;
    let fields = constructor
        .arguments
        .iter()
        .map(|r| {
            let type_ = type_to_fsharp(&r.type_);
            match &r.label {
                Some((_, ref label)) => docvec![label, ": ", type_],
                None => type_,
            }
        })
        .collect::<Vec<Document<'a>>>();

    docvec![
        "type ",
        map_publicity(t.publicity),
        name,
        type_params(t),
        " = ",
        join(fields, "; ".to_doc()).group().surround("{ ", " }"),
    ]
}

fn type_params(t: &CustomType<Arc<Type>>) -> Document<'_> {
    let type_params = join(
        t.typed_parameters.iter().map(|tp| type_to_fsharp(tp)),
        ", ".to_doc(),
    )
    .surround("<", ">");

    if !t.typed_parameters.is_empty() {
        type_params
    } else {
        "".to_doc()
    }
}

fn discriminated_union<'a>(t: &'a CustomType<Arc<Type>>) -> Document<'a> {
    println!("discriminated_union: {}", t.name);
    // need to track which named fields have the same name and position and generate method accessors for them

    // For this type:
    // Each constructor maps to a union case
    // For each union case where all cases have a field of the same type in the same position,
    // generate a method accessor for that field
    // So for this type in gleam:
    // ```gleam
    //     pub type Foo {
    //    Bar(name: String, age: Int)
    //    Baz(quux: Int)
    // }
    // ```
    // We generate this in F#
    // ```fsharp
    // type Foo =
    //     | Bar of name: string * age: int
    //     | Baz of name: string
    //     member this.getname() =
    //         match this with
    //         | Bar(name, _) -> name
    //         | Baz(name) -> name
    // ```
    let mut named_fields = HashMap::new();

    let mut max_indices = vec![];

    let type_name = &t.name;
    let constructors = t
        .constructors
        .iter()
        .map(|constructor| {
            let constructor_name = constructor.name.clone();
            let constructor_name_doc = constructor_name.to_doc();

            let mut field_index = 0;
            let fields = constructor
                .arguments
                .iter()
                .map(|r| {
                    let type_ = type_to_fsharp(&r.type_);
                    // Need to wrap in parens if it's a function type
                    let type_doc = if r.type_.is_fun() {
                        type_.surround("(", ")")
                    } else {
                        type_
                    };

                    let res = match &r.label {
                        Some((_, ref arg_name)) => {
                            named_fields
                                .entry(arg_name.clone())
                                .or_insert(Vec::new())
                                .push((field_index, r.type_.clone(), constructor));

                            docvec![arg_name, ": ", type_doc]
                        }
                        None => type_doc,
                    };

                    field_index += 1;
                    res
                })
                .collect::<Vec<Document<'a>>>();

            max_indices.push(constructor.arguments.len() - 1);
            if fields.is_empty() {
                constructor_name_doc
            } else {
                docvec![constructor_name_doc, " of ", join(fields, " * ".to_doc())]
            }
        })
        .collect::<Vec<Document<'a>>>();

    let member_declarations = named_fields.iter().filter_map(|(label, type_loc_list)| {
        if type_loc_list.len() == t.constructors.len() {
            let mut i = 0;
            let mut current_value = None;
            for (next_index, next_type, _) in type_loc_list {
                match current_value {
                    Some((index, type_)) => {
                        if next_type != type_ {
                            break;
                        }
                        if next_index != index {
                            break;
                        }
                    }
                    None => current_value = Some((next_index, next_type)),
                }
                i += 1;
            }

            if true {
                let cases = join(
                    type_loc_list.iter().map(|(index, _, constructor)| {
                        let max_index = constructor.arguments.len() - 1; // max_indices.get(*index).expect("Index out of bounds");

                        println!(
                            "index: {}, max_index: {}, max_indices: {:?}, constructor.arguments: {:#?}",
                            index, max_index, max_indices, constructor.arguments
                        );

                        let mut discards = (0..=max_index)
                            .map(|_| "_".to_doc())
                            .collect::<Vec<Document<'a>>>();

                        discards[*index] = label.to_doc();

                        let discards_doc = join(discards, ", ".to_doc()).surround("(", ")").group();

                        // let pattern = before_discards
                        //     .append(label)
                        //     .append(after_discards)
                        //     .surround("(", ")")
                        //     .group();
                        docvec![
                            "| ",
                            type_name.clone(),
                            ".",
                            constructor.name.clone().to_doc(),
                            " ",
                            discards_doc,
                            " -> ",
                            label,
                        ]
                    }),
                    line(),
                );

                return Some(docvec![
                    "member this.",
                    label,
                    // ": ",
                    // return_type,
                    " = ",
                    line().append(docvec!["match this with ", line(), cases, line()]),
                ]);
            }
        }
        None
    });

    let member_declarations_doc = join(member_declarations, line()).nest(INDENT).nest(INDENT);

    println!("member_declarations: {:#?}", &member_declarations_doc);

    docvec![
        "type ",
        map_publicity(t.publicity),
        type_name,
        type_params(t),
        " =",
        line(),
        join(
            constructors.into_iter().map(|c| docvec!["| ".to_doc(), c]),
            line()
        ),
        line().nest(INDENT),
        member_declarations_doc,
    ]
}

fn type_alias(t: &TypeAlias<Arc<Type>>) -> Document<'_> {
    docvec![
        "type ",
        map_publicity(t.publicity),
        t.alias.clone(),
        " = ",
        type_to_fsharp(&t.type_)
    ]
}

fn function(f: &TypedFunction) -> Document<'_> {
    let Function {
        name,
        arguments,
        body,
        return_type,
        ..
    } = f;

    let name = name.as_ref().map(|n| n.1.as_str()).unwrap_or("_");
    let args = if arguments.is_empty() {
        "()".to_doc()
    } else {
        fun_args(arguments)
    };

    let body = statements(body, Some(return_type));
    let return_type = type_to_fsharp(return_type);

    docvec![
        "let ",
        map_publicity(f.publicity),
        name,
        " ",
        args,
        ": ",
        return_type,
        " = begin",
        line().append(body).nest(INDENT).group(),
        line(),
        "end"
    ]
}
fn fun_args(arguments: &[TypedArg]) -> Document<'_> {
    join(
        arguments.iter().map(|arg| {
            docvec![
                arg.names
                    .get_variable_name()
                    .map(|n| n.to_doc())
                    .unwrap_or_else(|| "_".to_doc()),
                ": ",
                type_to_fsharp(&arg.type_)
            ]
            .surround("(", ")")
        }),
        " ".to_doc(),
    )
    .group()
}

/// Anonymous functions
fn fun<'a>(args: &'a [TypedArg], body: &'a [TypedStatement]) -> Document<'a> {
    "fun"
        .to_doc()
        .append(fun_args(args).append(" -> begin "))
        .append(statements(body, None).nest(INDENT))
        .append(break_("", " "))
        .append("end")
        .group()
}

fn statement(s: &TypedStatement) -> Document<'_> {
    match s {
        TypedStatement::Expression(expr) => expression(expr),
        TypedStatement::Assignment(a) => assignment(a),
        TypedStatement::Use(_) => docvec!["// TODO: Implement use statements"],
    }
}

fn assignment(assignment: &TypedAssignment) -> Document<'_> {
    let (name, value) = get_assignment_info(assignment);
    "let ".to_doc().append(name).append(" = ").append(value)
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
            "True" => "true".to_doc(),
            "False" => "false".to_doc(),
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
        } => case(subjects, clauses),

        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(expression)),

        // Special case for double negation
        TypedExpr::NegateInt { value, .. } => {
            // Special case for double negation
            if let TypedExpr::NegateInt { .. } = value.as_ref() {
                "-".to_doc().append(expression(value).surround("(", ")"))
            } else {
                "-".to_doc().append(expression(value))
            }
        }

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

fn tuple<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, ", ".to_doc()).surround("(", ")")
}
fn case<'a>(subjects: &'a [TypedExpr], clauses_: &'a [TypedClause]) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        expression(
            subjects
                .first()
                .expect("f# case printing of single subject"),
        )
    } else {
        tuple(subjects.iter().map(expression))
    };

    let mut res = "match "
        .to_doc()
        .append(subjects_doc)
        .append(" with")
        .append(line())
        .append(clauses(clauses_))
        .group();

    res
}

fn clauses(clauses: &[TypedClause]) -> Document<'_> {
    join(
        clauses
            .iter()
            .map(|c| "| ".to_doc().append(clause(c).group())),
        line(),
    )
}

fn clause(clause: &TypedClause) -> Document<'_> {
    let Clause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = clause;
    let mut then_doc = None;

    let mut additional_guards = vec![];
    let patterns_doc = join(
        std::iter::once(pat)
            .chain(alternative_patterns)
            .map(|patterns| {
                let patterns_doc = if patterns.len() == 1 {
                    let p = patterns.first().expect("Single pattern clause printing");
                    pattern(p)
                } else {
                    tuple(patterns.iter().map(pattern))
                };

                patterns_doc
            }),
        " | ".to_doc(),
    );
    let guard = optional_clause_guard(guard.as_ref(), additional_guards);
    if then_doc.is_none() {
        then_doc = Some(clause_consequence(then));
    }
    patterns_doc.append(
        guard.append(" ->").append(
            line()
                .append(then_doc.clone().to_doc())
                .nest(INDENT)
                .group(),
        ),
    )
}

fn clause_consequence(consequence: &TypedExpr) -> Document<'_> {
    match consequence {
        TypedExpr::Block { statements, .. } => statement_sequence(statements),
        _ => expression(consequence),
    }
}

fn statement_sequence(statements: &[TypedStatement]) -> Document<'_> {
    let count = statements.len();
    let mut documents = Vec::with_capacity(count * 3);
    for (i, expression) in statements.iter().enumerate() {
        documents.push(statement(expression).group());

        if i + 1 < count {
            // This isn't the final expression so add the delimeters
            documents.push(",".to_doc());
            documents.push(line());
        }
    }
    if count == 1 {
        documents.to_doc()
    } else {
        documents.to_doc().force_break()
    }
}
fn optional_clause_guard<'a>(
    guard: Option<&'a TypedClauseGuard>,
    additional_guards: Vec<Document<'a>>,
) -> Document<'a> {
    let guard_doc = guard.map(|guard| bare_clause_guard(guard));

    let guards_count = guard_doc.iter().len() + additional_guards.len();
    let guards_docs = additional_guards.into_iter().chain(guard_doc).map(|guard| {
        if guards_count > 1 {
            guard.surround("(", ")")
        } else {
            guard
        }
    });
    let doc = join(guards_docs, " andalso ".to_doc());
    if doc.is_empty() {
        doc
    } else {
        " when ".to_doc().append(doc)
    }
}
fn clause_guard(guard: &TypedClauseGuard) -> Document<'_> {
    match guard {
        // Binary operators are wrapped in parens
        ClauseGuard::Or { .. }
        | ClauseGuard::And { .. }
        | ClauseGuard::Equals { .. }
        | ClauseGuard::NotEquals { .. }
        | ClauseGuard::GtInt { .. }
        | ClauseGuard::GtEqInt { .. }
        | ClauseGuard::LtInt { .. }
        | ClauseGuard::LtEqInt { .. }
        | ClauseGuard::GtFloat { .. }
        | ClauseGuard::GtEqFloat { .. }
        | ClauseGuard::LtFloat { .. }
        | ClauseGuard::LtEqFloat { .. }
        | ClauseGuard::AddInt { .. }
        | ClauseGuard::AddFloat { .. }
        | ClauseGuard::SubInt { .. }
        | ClauseGuard::SubFloat { .. }
        | ClauseGuard::MultInt { .. }
        | ClauseGuard::MultFloat { .. }
        | ClauseGuard::DivInt { .. }
        | ClauseGuard::DivFloat { .. }
        | ClauseGuard::RemainderInt { .. } => {
            "(".to_doc().append(bare_clause_guard(guard)).append(")")
        }

        // Other expressions are not
        ClauseGuard::Constant(_)
        | ClauseGuard::Not { .. }
        | ClauseGuard::Var { .. }
        | ClauseGuard::TupleIndex { .. }
        | ClauseGuard::FieldAccess { .. }
        | ClauseGuard::ModuleSelect { .. } => bare_clause_guard(guard),
    }
}
fn bare_clause_guard(guard: &TypedClauseGuard) -> Document<'_> {
    match guard {
        ClauseGuard::Not { expression, .. } => docvec!["not ", bare_clause_guard(expression)],

        ClauseGuard::Or { left, right, .. } => clause_guard(left)
            .append(" || ")
            .append(clause_guard(right)),

        ClauseGuard::And { left, right, .. } => clause_guard(left)
            .append(" && ")
            .append(clause_guard(right)),

        ClauseGuard::Equals { left, right, .. } => {
            clause_guard(left).append(" = ").append(clause_guard(right))
        }

        ClauseGuard::NotEquals { left, right, .. } => clause_guard(left)
            .append(" <> ")
            .append(clause_guard(right)),

        ClauseGuard::GtInt { left, right, .. } | ClauseGuard::GtFloat { left, right, .. } => {
            clause_guard(left).append(" > ").append(clause_guard(right))
        }

        ClauseGuard::GtEqInt { left, right, .. } | ClauseGuard::GtEqFloat { left, right, .. } => {
            clause_guard(left)
                .append(" >= ")
                .append(clause_guard(right))
        }

        ClauseGuard::LtInt { left, right, .. } | ClauseGuard::LtFloat { left, right, .. } => {
            clause_guard(left).append(" < ").append(clause_guard(right))
        }

        ClauseGuard::LtEqInt { left, right, .. } | ClauseGuard::LtEqFloat { left, right, .. } => {
            clause_guard(left)
                .append(" =< ")
                .append(clause_guard(right))
        }

        ClauseGuard::AddInt { left, right, .. } | ClauseGuard::AddFloat { left, right, .. } => {
            clause_guard(left).append(" + ").append(clause_guard(right))
        }

        ClauseGuard::SubInt { left, right, .. } | ClauseGuard::SubFloat { left, right, .. } => {
            clause_guard(left).append(" - ").append(clause_guard(right))
        }

        ClauseGuard::MultInt { left, right, .. } | ClauseGuard::MultFloat { left, right, .. } => {
            clause_guard(left).append(" * ").append(clause_guard(right))
        }

        ClauseGuard::DivInt { left, right, .. } | ClauseGuard::DivFloat { left, right, .. } => {
            clause_guard(left).append(" / ").append(clause_guard(right))
        }

        ClauseGuard::RemainderInt { left, right, .. } => {
            clause_guard(left).append(" % ").append(clause_guard(right))
        }

        // TODO: Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => name.to_doc(),

        // ClauseGuard::TupleIndex { tuple, index, .. } => tuple_index_inline(tuple, *index),

        // ClauseGuard::FieldAccess {
        //     container, index, ..
        // } => tuple_index_inline(container, index.expect("Unable to find index") + 1),

        // ClauseGuard::ModuleSelect { literal, .. } => const_inline(literal),
        ClauseGuard::Constant(c) => inline_constant(c),
        _ => docvec!["// TODO: Implement other guard types"],
    }
}

fn inline_constant<'a>(c: &Constant<Arc<Type>, EcoString>) -> Document<'a> {
    match c {
        Constant::Int { value, .. } => value.to_doc(),
        Constant::Float { value, .. } => value.to_doc(),
        Constant::String { value, .. } => string(value.as_str()),
        Constant::Tuple { elements, .. } => tuple(elements.iter().map(inline_constant)),
        Constant::List { elements, .. } => {
            join(elements.iter().map(inline_constant), "; ".to_doc()).surround("[", "]")
        }

        _ => docvec!["// TODO: Implement other inline_constant types"],
    }
}

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

/// Implement pipeline (|>) expressions
fn pipeline<'a>(assignments: &'a [TypedAssignment], finally: &'a TypedExpr) -> Document<'a> {
    let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

    for a in assignments {
        let (name, value) = get_assignment_info(a);

        documents.push("let ".to_doc().append(name).append(" = ").append(value));
        documents.push(line());
    }

    documents.push(expression(finally).surround("(", ")"));

    wrap_in_begin_end(documents.to_doc())
}

fn wrap_in_begin_end(expr: Document<'_>) -> Document<'_> {
    "begin"
        .to_doc()
        .append(line())
        .nest(INDENT)
        .append(expr.nest(INDENT).group())
        .append(line().append("end"))
}

fn block(s: &[TypedStatement]) -> Document<'_> {
    "begin"
        .to_doc()
        .append(line())
        .nest(INDENT)
        .append(statements(s, None).nest(INDENT).group())
        .append(line().append("end"))
}

fn get_assignment_info(assignment: &TypedAssignment) -> (Document<'_>, Document<'_>) {
    let name = pattern(&assignment.pattern);
    let value = expression(&assignment.value);
    (name, value)
}

fn pattern(p: &Pattern<Arc<Type>>) -> Document<'_> {
    match p {
        Pattern::Int { value, .. } => value.to_doc(),
        Pattern::Float { value, .. } => value.to_doc(),
        Pattern::String { value, .. } => string(value.as_str()),
        Pattern::Variable { name, .. } => name.to_doc(),
        Pattern::Discard { name, .. } => name.to_doc(),
        Pattern::List { elements, tail, .. } => {
            let head = join(elements.iter().map(pattern), "; ".to_doc()).surround("[", "]");

            match tail {
                Some(tail) => head.append("::").append(pattern(tail)),
                None => head,
            }
        }
        Pattern::Tuple { elems, .. } => {
            join(elems.iter().map(pattern), ", ".to_doc()).surround("(", ")")
        }

        Pattern::StringPrefix {
            left_side_string,
            right_side_assignment,
            left_side_assignment,
            ..
        } if left_side_assignment.is_none() => {
            let right = match right_side_assignment {
                AssignName::Variable(right) => right.to_doc(),
                AssignName::Discard(_) => "_".to_doc(),
            };

            // Use an active pattern helper function defined in prelude.fs
            prelude_functions::STRING_PATTERN_PREFIX
                .to_doc()
                .append(" ")
                .append(string(left_side_string))
                .append(" ")
                .append(right)

            // string(left_side_string).append(" + ").append(right)

            // match left_side_assignment {
            //     Some((left_name, _)) => {
            //         // "wibble" as prefix <> rest
            //         //             ^^^^^^^^^ In case the left prefix of the pattern matching is given an alias
            //         //                       we bind it to a local variable so that it can be correctly
            //         //                       referenced inside the case branch.
            //         //
            //         // <<Prefix:3/binary, Rest/binary>> when Prefix =:= <<"wibble">>
            //         //   ^^^^^^^^                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            //         //   since F#'s binary pattern matching doesn't allow direct string assignment
            //         //   to variables within the pattern, we first match the expected prefix length in
            //         //   bytes, then use a guard clause to verify the content.
            //         //
            //         let name = left_name.to_doc();
            //         //guards.push(docvec![name.clone(), " =:= ", string(left_side_string)]);
            //         docvec![
            //             "<<",
            //             name.clone(),
            //             ":",
            //             string_length_utf8_bytes(left_side_string),
            //             "/binary",
            //             ", ",
            //             right,
            //             "/binary>>",
            //         ]
            //     }
            //     None => docvec![
            //         "<<\"",
            //         string_inner(left_side_string),
            //         "\"/utf8",
            //         ", ",
            //         right,
            //         "/binary>>"
            //     ],
            // }
        }
        Pattern::StringPrefix { .. } => "// Prefix assignment pattern not yet supported".to_doc(),
        Pattern::BitArray { segments, .. } => {
            let segments_docs = segments.iter().map(|s| pattern(&s.value));
            join(segments_docs, "; ".to_doc()).surround("[|", "|]")
        }
        Pattern::VarUsage {
            name, constructor, ..
        } => {
            let v = &constructor
                .as_ref()
                .expect("Constructor not found for variable usage")
                .variant;
            match v {
                ValueConstructorVariant::ModuleConstant { literal, .. } => inline_constant(literal),
                _ => name.to_doc(),
            }
        }
        Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
        Pattern::Assign {
            name,
            location,
            pattern: p,
        } => pattern(p).append(" as ").append(name),
        Pattern::Constructor {
            location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        } => {
            let name = name.to_doc();
            let args = arguments.iter().map(|arg| pattern(&arg.value));
            let args = if arguments.is_empty() {
                "()".to_doc()
            } else {
                join(args, "; ".to_doc()).surround("(", ")")
            };
            docvec![name, args]
        }
    }
}

fn type_to_fsharp<'a>(t: &Type) -> Document<'a> {
    if t.is_nil() {
        return "unit".to_doc();
    }

    match t {
        Type::Named { name, args, .. } => {
            let name = match name.as_str() {
                "Int" | "int" => "int".to_doc(),
                "Float" | "float" => "float".to_doc(),
                "String" | "string" => "string".to_doc(),
                "Bool" | "bool" => "bool".to_doc(),
                "Nil" => "unit".to_doc(),
                "List" => "list".to_doc(),
                _ => name.to_doc(),
            };
            if args.is_empty() {
                name.to_doc()
            } else {
                name.to_doc()
                    .append("<")
                    .append(join(
                        args.iter().map(|arg| type_to_fsharp(arg)),
                        ", ".to_doc(),
                    ))
                    .append(">")
            }
        }
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

    match constant.value.deref() {
        Constant::Int { .. } | Constant::Float { .. } | Constant::String { .. } => {
            docvec![
                "[<Literal>]",
                line(),
                "let ",
                map_publicity(constant.publicity),
                name,
                " = ",
                constant_expression(&constant.value)
            ]
        }
        _ => docvec![
            "let ",
            map_publicity(constant.publicity),
            name,
            " = ",
            constant_expression(&constant.value)
        ],
    }
    // match value.deref().clone() {
    //     Constant::Int { value, .. } | Constant::Float { value, .. } => {
    //         annotation.append(binding.append(value.to_doc()))
    //     }
    //     Constant::String { value, .. } => annotation.append(binding.append(string(value.as_str()))),
    //     Constant::Tuple { elements, .. } => {
    //         binding.append(tuple(elements.iter().map(inline_constant)))
    //     }
    //     Constant::List { elements, .. } => {
    //         binding.append(join(elements.iter().map(inline_constant), "; ".to_doc()))
    //     }
    //     Constant::Record {
    //         location,
    //         module,
    //         name,
    //         args,
    //         tag,
    //         type_,
    //         field_map,
    //     } => {
    //         println!("module: {:#?}", module);
    //         println!("name: {:#?}", name);
    //         println!("args: {:#?}", args);
    //         println!("tag: {:#?}", tag);
    //         println!("type_: {:#?}", type_);
    //         println!("field_map: {:#?}", field_map);
    //         todo!()
    //     }
    //     Constant::BitArray { location, segments } => todo!(),
    //     Constant::Var {
    //         location,
    //         module,
    //         name,
    //         constructor,
    //         type_,
    //     } => todo!(),
    //     Constant::StringConcatenation {
    //         location,
    //         left,
    //         right,
    //     } => todo!(),
    //     Constant::Invalid { location, type_ } => todo!(),
    //}
}

pub(crate) fn constant_expression<'a>(expression: &'a TypedConstant) -> Document<'a> {
    match expression {
        Constant::Int { value, .. } => value.to_doc(),
        Constant::Float { value, .. } => value.to_doc(),
        Constant::String { value, .. } => string(value),
        Constant::Tuple { elements, .. } => tuple(elements.iter().map(|e| constant_expression(e))),

        Constant::List { elements, .. } => {
            //tracker.list_used = true;
            let list = list(elements.iter().map(|e| constant_expression(e)));

            list
            // match context {
            //     Context::Constant => Ok(docvec!["/* @__PURE__ */ ", list]),
            //     Context::Function => Ok(list),
            // }
        }

        Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
            "true".to_doc()
        }
        Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
            "false".to_doc()
        }
        Constant::Record { type_, .. } if type_.is_nil() => "()".to_doc(),

        Constant::Record {
            args,
            module,
            name,
            tag,
            type_,
            ..
        } => {
            // if type_.is_result() {
            //     if tag == "Ok" {
            //         tracker.ok_used = true;
            //     } else {
            //         tracker.error_used = true;
            //     }
            // }

            // If there's no arguments and the type is a function that takes
            // arguments then this is the constructor being referenced, not the
            // function being called.
            if let Some(arity) = type_.fn_arity() {
                if args.is_empty() && arity != 0 {
                    let arity = arity as u16;
                    return record_constructor(type_.clone(), None, name, arity);
                }
            }

            let field_values: Vec<_> = args
                .iter()
                .map(|arg| constant_expression(&arg.value))
                .collect(); //.try_collect()
                            //.expect("failed to collect field values");

            construct_record(
                module.as_ref().map(|(module, _)| module.as_str()),
                name,
                field_values,
            )
            // match context {
            //     Context::Constant => Ok(docvec!["/* @__PURE__ */ ", constructor]),
            //     Context::Function => Ok(constructor),
            // }
        }

        Constant::BitArray { segments, .. } => {
            todo!()
            // let bit_array = bit_array(segments, |expr| constant_expression(expr))?;
            // match context {
            //     Context::Constant => Ok(docvec!["/* @__PURE__ */ ", bit_array]),
            //     Context::Function => Ok(bit_array),
            // }
        }

        Constant::Var { name, module, .. } => {
            match module {
                None => santitize_name(name),
                Some((module, _)) => {
                    // JS keywords can be accessed here, but we must escape anyway
                    // as we escape when exporting such names in the first place,
                    // and the imported name has to match the exported name.
                    docvec![module, ".", santitize_name(name)]
                }
            }
        }

        Constant::StringConcatenation { left, right, .. } => {
            let left = constant_expression(left);
            let right = constant_expression(right);
            docvec!(left, " + ", right)
        }

        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
    }
}

fn record_constructor<'a>(
    type_: Arc<Type>,
    qualifier: Option<&'a str>,
    name: &'a str,
    arity: u16,
) -> Document<'a> {
    // if qualifier.is_none() && type_.is_result_constructor() {
    //     if name == "Ok" {
    //         tracker.ok_used = true;
    //     } else if name == "Error" {
    //         tracker.error_used = true;
    //     }
    // }
    if type_.is_bool() && name == "True" {
        "true".to_doc()
    } else if type_.is_bool() {
        "false".to_doc()
    } else if type_.is_nil() {
        "undefined".to_doc()
    } else if arity == 0 {
        match qualifier {
            Some(module) => docvec![module, ".", name, "()"],
            None => docvec![name, "()"],
        }
    } else {
        let vars = (0..arity).map(|i| Document::String(format!("var{i}")));
        let body = construct_record(qualifier, name, vars.clone());

        docvec!("fun ", wrap_args(vars), " -> begin", break_("", " "), body)
            .nest(INDENT)
            .append(break_("", " "))
            .group()
            .append("end")
    }
}
fn wrap_args<'a>(args: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    break_("", "")
        .append(join(args, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn construct_record<'a>(
    module: Option<&'a str>,
    name: &'a str,
    arguments: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    let mut any_arguments = false;
    let arguments = join(
        arguments.into_iter().inspect(|_| {
            any_arguments = true;
        }),
        break_(",", ", "),
    );
    let arguments = docvec![break_("", ""), arguments].nest(INDENT);
    let name = if let Some(module) = module {
        docvec![module, ".", name]
    } else {
        name.to_doc()
    };
    if any_arguments {
        docvec![name, "(", arguments, break_(",", ""), ")"].group()
    } else {
        docvec![name, "()"]
    }
}
fn list<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, "; ".to_doc()).group().surround("[", "]")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
}
