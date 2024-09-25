#[cfg(test)]
mod tests;

use crate::{
    analyse::Inferred,
    ast::*,
    docvec,
    pretty::*,
    type_::{
        Deprecation, FieldMap, PatternConstructor, Type, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
};
use ecow::EcoString;
use itertools::Itertools;
use regex::{Captures, Regex};
use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, OnceLock},
};

const INDENT: isize = 4;
pub const FSHARP_PRELUDE: &str = include_str!("./fsharp/prelude.fs");

struct Generator {}

mod prelude_functions {
    /// This is used directly in pattern matching
    pub const STRING_PATTERN_PREFIX: &str = "Gleam__codegen__prefix";

    // TODO: Is it worth it to have two separate functions here?
    // Probably not

    /// This is used directly in pattern matching
    pub const STRING_PATTERN_PARTS: &str = "Gleam_codegen_string_parts";
}

pub fn render_module(module: &TypedModule) -> super::Result<String> {
    let document = join(
        vec![module_declaration(module), module_contents(module)],
        line(),
    );
    Ok(document.to_pretty_string(120))
}

fn module_declaration(module: &TypedModule) -> Document<'_> {
    //println!("module: {:#?}", module);
    // Use module rec to not need to worry about initialization order
    "module rec ".to_doc().append(santitize_name(&module.name))
}

fn santitize_name(name: &EcoString) -> Document<'_> {
    join(
        name.split("/").map(|s| {
            if is_reserved_word(s) {
                s.to_doc().surround("``", "``")
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
                Definition::CustomType(t) => {
                    if t.constructors.len() == 1 {
                        // Might be an F# record type, but only if the constructor name
                        // matches the type name and all constructor arguments have a label
                        let c = t.constructors.first().expect("There must be a constructor");
                        if c.name == t.name
                            && !c.arguments.is_empty()
                            && c.arguments.iter().all(|a| a.label.is_some())
                            && c.arguments.len() > 1
                        {
                            return record_type(t);
                        }
                    }
                    discriminated_union(t)
                }
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
        Publicity::Public => nil(),
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
    assert!(
        t.constructors.len() == 1,
        "Gleam records must have a single constructor to count as a record"
    );

    let constructor = t
        .constructors
        .first()
        .expect("Single constructor should exist");

    // If a gleam record has an argument with 0 arguments, we need to treat this type like a DU
    if constructor.arguments.is_empty() {
        return discriminated_union(t);
    }

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
        nil()
    }
}

fn discriminated_union<'a>(t: &'a CustomType<Arc<Type>>) -> Document<'a> {
    // need to track which named fields have the same name and position and generate method accessors for them

    let mut named_fields = HashMap::new();

    let mut max_indices = vec![];

    let type_name = &t.name;
    // Need to sort the constructors before mapping to ensure repeatable output
    let constructors = t
        .constructors
        .iter()
        .sorted_by(|a, b| Ord::cmp(&a.name, &b.name))
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

            if !constructor.arguments.is_empty() {
                max_indices.push(constructor.arguments.len() - 1);
            }
            if fields.is_empty() {
                constructor_name_doc
            } else {
                docvec![constructor_name_doc, " of ", join(fields, " * ".to_doc())]
            }
        })
        .collect::<Vec<Document<'a>>>();

    let member_declarations = named_fields
        .iter()
        .sorted_by(|a, b| Ord::cmp(&a.0, &b.0))
        .filter_map(|(label, type_loc_list)| {
            if type_loc_list.len() == t.constructors.len() {
                let (first_index, first_type, _) = type_loc_list
                    .first()
                    .expect("Type loc list should have elements");

                let meets_requirements = type_loc_list
                    .iter()
                    .all(|(index, type_, _)| index == first_index && type_ == first_type);

                if meets_requirements {
                    let cases = type_loc_list
                        .iter()
                        .sorted_by(|a, b| Ord::cmp(&a.2.name, &b.2.name))
                        .map(|(index, _, constructor)| {
                            let max_index = constructor.arguments.len() - 1;

                            let mut discards = (0..=max_index)
                                .map(|_| "_".to_doc())
                                .collect::<Vec<Document<'a>>>();

                            *discards.get_mut(*index).expect("Index out of bounds") =
                                label.to_doc();

                            let discards_doc =
                                join(discards, ", ".to_doc()).surround("(", ")").group();

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
                        })
                        .collect::<Vec<_>>();

                    return Some(
                        docvec![
                            "member this.",
                            label,
                            " = ",
                            line(),
                            "match this with",
                            line(),
                            join(cases, line()),
                        ]
                        .group()
                        .nest(INDENT),
                    );
                }
            }
            None
        });

    let member_declarations_doc = join(member_declarations, line()).nest(INDENT);

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
        member_declarations_doc
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
        documentation,
        deprecation,
        external_fsharp,
        ..
    } = f;
    let name = name.as_ref().map(|n| n.1.as_str()).unwrap_or("_");

    let body = match external_fsharp {
        Some((ref module_name, ref fn_name, _)) => {
            // TODO: look into tracking references to the external function in Call expressions
            // and call them directly instead? Or maybe add an inline specifier for this [instead]?

            let calling_args = if arguments.is_empty() {
                "()".to_doc()
            } else {
                join(arguments.iter().map(|arg| arg_name(arg)), " ".to_doc())
            };

            // If the "module" qualifier is a file path, assume that fn_name is fully qualified
            let qualifier = if module_name.contains("/") || module_name.contains("\\") {
                nil()
            } else {
                docvec![module_name, "."]
            };

            docvec![qualifier, fn_name, " ", calling_args]
        }

        None => {
            docvec![
                "begin",
                line()
                    .append(statements(body, Some(return_type)))
                    .nest(INDENT)
                    .group(),
                line(),
                "end"
            ]
        }
    };

    let args = fun_args(arguments);
    let docs = match documentation {
        Some((_, ref doc)) => {
            let mut comment_lines = doc.split('\n').collect::<Vec<_>>();

            // Remove any trailing empty lines
            if comment_lines.last().map_or(false, |line| line.is_empty()) {
                _ = comment_lines.pop();
            }

            join(
                comment_lines
                    .iter()
                    .map(|doc_line| docvec!["///", doc_line]),
                line(),
            )
            .append(line())
        }
        None => nil(),
    };

    let deprecation_doc = match deprecation {
        Deprecation::Deprecated { message } => {
            docvec!["[<System.Obsolete(", string(message), ")>]", line()]
        }
        Deprecation::NotDeprecated => nil(),
    };

    let return_type = type_to_fsharp(return_type);

    // For now, since we mark all modules as recursive, we don't need to mark
    // functions as recursive.
    docvec![
        docs,
        deprecation_doc,
        "let ",
        map_publicity(f.publicity),
        name,
        " ",
        args,
        ": ",
        return_type,
        " = ",
        body
    ]
}

fn arg_name(arg: &TypedArg) -> Document<'_> {
    arg.names
        .get_variable_name()
        .map(|n| n.to_doc())
        .unwrap_or_else(|| "_".to_doc())
}

/// Function definition arguments
fn fun_args(arguments: &[TypedArg]) -> Document<'_> {
    if arguments.is_empty() {
        "()".to_doc()
    } else {
        join(
            arguments.iter().map(|arg| {
                docvec![arg_name(arg), ": ", type_to_fsharp(&arg.type_)].surround("(", ")")
            }),
            " ".to_doc(),
        )
        .group()
    }
}

/// Anonymous functions
fn fun<'a>(args: &'a [TypedArg], body: &'a [TypedStatement]) -> Document<'a> {
    docvec![
        "fun",
        fun_args(args),
        " -> begin ",
        statements(body, None).nest(INDENT),
        break_("", " "),
        "end"
    ]
    .group()
}

fn statement(s: &TypedStatement) -> (Document<'_>, Option<Document<'_>>) {
    let mut last_var = None;
    let statement_doc = match s {
        Statement::Expression(expr) => {
            last_var = None;
            expression(expr)
        }
        TypedStatement::Assignment(Assignment {
            value,
            pattern:
                Pattern::StringPrefix {
                    left_side_string: prefix,
                    right_side_assignment,
                    left_side_assignment: maybe_label,
                    ..
                },
            ..
        }) => {
            // TODO: Add warning suppression when this is encountered:
            // #nowarn "25" // Incomplete pattern matches on this expression.
            let suffix_binding_name: Document<'_> = match right_side_assignment {
                AssignName::Variable(right) => {
                    let v = right.to_doc();

                    last_var = Some(v.clone());
                    v
                }
                AssignName::Discard(_) => {
                    last_var = Some(expression(value).to_doc());
                    "_".to_doc()
                }
            };

            docvec![
                "let (",
                prelude_functions::STRING_PATTERN_PARTS,
                " ",
                string(prefix),
                " (",
                match maybe_label {
                    Some((prefix_label, _)) => docvec![prefix_label, ", "],
                    None => "_, ".to_doc(),
                },
                suffix_binding_name,
                ")) = ",
                expression(value).to_doc(),
            ]
        }

        Statement::Assignment(a) => {
            let (name, value) = get_assignment_info(a);

            last_var = Some(name.clone());
            match a.value.as_ref() {
                TypedExpr::Case { .. } => {
                    docvec![
                        "let ",
                        name,
                        " = ",
                        line().nest(INDENT),
                        value.group().nest(INDENT)
                    ]
                }
                _ => docvec!["let ", name, " = ", value],
            }
        }
        Statement::Use(_) => docvec!["// TODO: Implement use statements"],
    };

    (statement_doc, last_var)
}

fn statements<'a>(s: &'a [TypedStatement], return_type: Option<&Type>) -> Document<'a> {
    let mut last_var = None;
    let mut res = s
        .iter()
        .map(|s| {
            let statement_info = statement(s);
            last_var = statement_info.1;
            statement_info.0
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
        Regex::new(r#"(\\+)(u)\{(\S+)\}"#)
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

        TypedExpr::Call { fun, args, .. } => match fun.as_ref() {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::Record {
                                field_map: Some(ref field_map),
                                ..
                            },
                        ..
                    },
                ..
            } => record_instantiation(field_map, args),
            _ => function_call(fun, args),
        },

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
        TypedExpr::RecordAccess { label, record, .. } => record_access(record, label),
        TypedExpr::RecordUpdate { args, spread, .. } => {
            // If the target of the update is the result of a pipeline, it needs to be
            // surrounded in parentheses
            let old_var_name = match spread.deref() {
                TypedExpr::Pipeline { .. } => expression(spread).surround("(", ")"),
                _ => expression(spread),
            };

            let new_values = args.iter().map(|arg| {
                let child_expr = match &arg.value {
                    // If the child here is a pipe operation, we need to indent at least
                    // one more space so that it starts on a column after the `with` keyword
                    TypedExpr::Pipeline { .. } => expression(&arg.value).nest(1),
                    _ => expression(&arg.value),
                };

                docvec![arg.label.clone(), " = ", child_expr].group()
            });

            docvec![
                "{ ",
                old_var_name,
                " with ",
                join(new_values, "; ".to_doc()).force_break(),
                " }"
            ]
        }
        TypedExpr::ModuleSelect { .. } => "// TODO: TypedExpr::ModuleSelect".to_doc(),
        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, index),
        TypedExpr::BitArray { .. } => "// TODO: TypedExpr::BitArray".to_doc(),
        TypedExpr::NegateBool { .. } => "// TODO: TypedExpr::NegateBool".to_doc(),
        TypedExpr::Invalid { .. } => "// TODO: TypedExpr::Invalid".to_doc(),
    }
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: &'a u64) -> Document<'a> {
    // TODO: Add warning suppression when this is encountered:
    // #nowarn "3220" // This method or property is not normally used from F# code, use an explicit tuple pattern for deconstruction instead.
    docvec![expression(tuple), ".Item", index + 1]
}

fn invert_field_map(field_map: &FieldMap) -> HashMap<&u32, &EcoString> {
    field_map
        .fields
        .iter()
        .map(|(k, v)| (v, k))
        .collect::<HashMap<_, _>>()
}

fn record_instantiation<'a>(
    field_map: &'a FieldMap,
    args: &'a [CallArg<TypedExpr>],
) -> Document<'a> {
    let field_map = invert_field_map(field_map);

    let args = args.iter().enumerate().map(|(i, arg)| {
        let label = field_map.get(&(i as u32)).expect("Index out of bounds");
        docvec![label.to_doc(), " = ", expression(&arg.value)]
    });

    join(args, "; ".to_doc()).group().surround("{ ", " }")
}

fn function_call<'a>(fun: &'a TypedExpr, args: &'a [CallArg<TypedExpr>]) -> Document<'a> {
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
    let fun_expr = match fun {
        TypedExpr::Fn { .. } => fun_expr.surround("(", ")"),
        _ => fun_expr,
    };
    fun_expr.append(args).group()
}

fn record_access<'a>(record: &'a TypedExpr, label: &'a EcoString) -> Document<'a> {
    let record_expr = expression(record);
    let label_expr = label.to_doc();
    docvec![record_expr, ".", label_expr]
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
fn case<'a>(subjects: &'a [TypedExpr], clauses: &'a [TypedClause]) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        expression(
            subjects
                .first()
                .expect("f# case printing of single subject"),
        )
    } else {
        tuple(subjects.iter().map(expression))
    };

    let clauses = join(
        clauses
            .iter()
            .map(|c| "| ".to_doc().append(clause(c).group())),
        line(),
    )
    .group();
    docvec![
        docvec!["match ", subjects_doc, " with"].group(),
        line(),
        clauses
    ]
    .group()
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

    let additional_guards = vec![];
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
    let documents = statements.iter().map(|e| statement(e).0.group());

    let documents = join(documents, line());
    if statements.len() == 1 {
        documents
    } else {
        documents.force_break()
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
    let doc = join(guards_docs, " && ".to_doc());
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

        //ClauseGuard::TupleIndex { tuple, index, .. } => tuple_index(tuple, index),

        // ClauseGuard::FieldAccess {
        //     container, index, ..
        // } => tuple_index_inline(container, index.expect("Unable to find index") + 1),

        // ClauseGuard::ModuleSelect { literal, .. } => const_inline(literal),
        ClauseGuard::Constant(c) => constant_expression(c),
        _ => docvec!["// TODO: Implement other guard types"],
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
            left_side_string: prefix,
            right_side_assignment,
            left_side_assignment: maybe_prefix_label,
            ..
        } => {
            // TODO: Add warning suppression when this is encountered:
            // #nowarn "25" // Incomplete pattern matches on this expression.
            let suffix_binding_name: Document<'_> = match right_side_assignment {
                AssignName::Variable(right) => right.to_doc(),
                AssignName::Discard(_) => "_".to_doc(),
            };

            match maybe_prefix_label {
                None => {
                    docvec![
                        prelude_functions::STRING_PATTERN_PREFIX,
                        " ",
                        string(prefix),
                        " ",
                        suffix_binding_name,
                    ]
                }
                Some((prefix_label, _)) => {
                    docvec![
                        prelude_functions::STRING_PATTERN_PARTS,
                        " ",
                        string(prefix),
                        " (",
                        prefix_label.to_doc(),
                        ", ",
                        suffix_binding_name,
                        ")"
                    ]
                }
            }
        }
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
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    constant_expression(literal)
                }
                _ => name.to_doc(),
            }
        }
        Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
        Pattern::Assign {
            name, pattern: p, ..
        } => pattern(p).append(" as ").append(name),

        Pattern::Constructor {
            constructor:
                Inferred::Known(PatternConstructor {
                    field_map: Some(ref field_map),
                    ..
                }),
            spread,
            arguments,
            ..
        } => {
            let field_map = invert_field_map(field_map);

            let args = arguments.iter().enumerate().filter_map(|(i, arg)| {
                if spread.is_some() && arg.value.is_discard() {
                    return None;
                }

                let label = match &arg.label {
                    Some(label) => label,
                    None => field_map.get(&(i as u32)).expect("Index out of bounds"),
                };

                Some(docvec![label.to_doc(), " = ", pattern(&arg.value)])
            });
            join(args, "; ".to_doc()).group().surround("{ ", " }")
        }

        Pattern::Constructor {
            name,
            arguments,
            //constructor,
            ..
        } => {
            let args = arguments.iter().map(|arg| pattern(&arg.value));
            let args = if arguments.is_empty() {
                "()".to_doc()
            } else {
                join(args, "; ".to_doc()).surround("(", ")")
            };
            docvec![name.to_doc(), args]
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
            field_map,
            ..
        } => {
            // If there's no arguments and the type is a function that takes
            // arguments then this is the constructor being referenced, not the
            // function being called.
            if let Some(arity) = type_.fn_arity() {
                if args.is_empty() && arity != 0 {
                    let arity = arity as u16;
                    return record_constructor(type_.clone(), None, name, arity);
                }
            }

            if field_map.is_none() && args.is_empty() {
                return tag.to_doc();
            }

            let field_values: Vec<_> = args
                .iter()
                .map(|arg| constant_expression(&arg.value))
                .collect();

            construct_record(
                module.as_ref().map(|(module, _)| module.as_str()),
                name,
                field_values,
            )
        }

        Constant::BitArray { .. } => "//TODO: Constant::BitArray".to_doc(),

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
