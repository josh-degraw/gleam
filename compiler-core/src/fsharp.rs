#[cfg(test)]
mod tests;

use crate::{
    analyse::Inferred,
    ast::*,
    docvec,
    pretty::*,
    type_::{
        Deprecation, FieldMap, PatternConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
        ValueConstructorVariant,
    },
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;
use regex::{Captures, Regex};
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::{Arc, OnceLock},
};
use vec1::Vec1;

const INDENT: isize = 4;
pub const FSHARP_PRELUDE: &str = include_str!("./fsharp/prelude.fs");

#[derive(Debug)]
pub struct Generator<'a> {
    package_name: &'a EcoString,
    pub external_files: HashSet<Utf8PathBuf>,
    module: &'a TypedModule,
    input_file_path: &'a Utf8PathBuf,
}

mod prelude_functions {
    /// This is used directly in pattern matching
    pub const STRING_PATTERN_PREFIX: &str = "Gleam__codegen__prefix";

    // TODO: Is it worth it to have two separate functions here?
    // Probably not

    /// This is used directly in pattern matching
    pub const STRING_PATTERN_PARTS: &str = "Gleam_codegen_string_parts";
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

            // Keywords
            | "abstract"
            | "and"
            | "as"
            | "assert"
            | "base"
            | "begin"
            | "class"
            | "default"
            | "delegate"
            | "do"
            | "done"
            | "downcast"
            | "downto"
            | "elif"
            | "else"
            | "end"
            | "exception"
            | "extern"
            | "false"
            | "finally"
            | "fixed"
            | "for"
            | "fun"
            | "function"
            | "global"
            | "if"
            | "in"
            | "inherit"
            | "inline"
            | "interface"
            | "internal"
            | "lazy"
            | "let"
            | "match"
            | "member"
            | "module"
            | "mutable"
            | "namespace"
            | "new"
            | "not"
            | "null"
            | "of"
            | "open"
            | "or"
            | "override"
            | "private"
            | "public"
            | "rec"
            | "return"
            | "select"
            | "static"
            | "struct"
            | "then"
            | "to"
            | "true"
            | "try"
            | "type"
            | "upcast"
            | "use"
            | "val"
            | "void"
            | "when"
            | "while"
            | "with"
            | "yield"
    )
}

fn unicode_escape_sequence_pattern() -> &'static Regex {
    static PATTERN: OnceLock<Regex> = OnceLock::new();
    PATTERN.get_or_init(|| {
        Regex::new(r#"(\\+)(u)\{(\S+)\}"#)
            .expect("Unicode escape sequence regex cannot be constructed")
    })
}

impl<'a> Generator<'a> {
    pub fn new(
        package_name: &'a EcoString,
        module: &'a TypedModule,
        input_file_path: &'a Utf8PathBuf,
    ) -> Self {
        Self {
            package_name,
            external_files: HashSet::new(),
            module,
            input_file_path,
        }
    }

    pub fn render(&mut self) -> super::Result<String> {
        let document = join(
            vec![
                self.module_declaration(),
                self.render_imports(),
                self.module_contents(),
            ],
            line(),
        );
        Ok(document.to_pretty_string(120))
    }

    /// Update the currently referenced module and render it
    pub fn render_module(
        &mut self,
        new_module: &'a TypedModule,
        input_file_path: &'a Utf8PathBuf,
    ) -> super::Result<String> {
        self.module = new_module;
        self.input_file_path = input_file_path;
        self.render()
    }

    fn module_declaration(&self) -> Document<'a> {
        //println!("module: {:#?}", module);
        // Use module rec to not need to worry about initialization order
        "module rec "
            .to_doc()
            .append(self.sanitize_name(&self.module.name))
    }

    fn render_imports(&self) -> Document<'a> {
        let all_imports = self
            .module
            .definitions
            .iter()
            .filter_map(|def| match def {
                Definition::Import(i) => Some(self.import(i)), // handled before this part to ensure order
                _ => None,
            })
            .reduce(|(mut a, mut b, mut c), (a1, b1, c1)| {
                a.extend(a1);
                b.extend(b1);
                c.extend(c1);
                (a, b, c)
            });

        match all_imports {
            Some((open_statements, module_aliases, other_aliases)) => join(
                [open_statements, module_aliases, other_aliases].concat(),
                line(),
            ),
            None => nil(),
        }
    }

    fn module_contents(&mut self) -> Document<'a> {
        join(
            self.module
                .definitions
                .iter()
                .map(|def| match def {
                    Definition::CustomType(t) => self.custom_type(t),
                    Definition::TypeAlias(t) => self.type_alias(t),
                    Definition::ModuleConstant(c) => self.module_constant(c),
                    Definition::Function(f) => self.function(f),
                    Definition::Import(i) => nil(), // handled before this part to ensure order
                })
                .collect::<Vec<Document<'a>>>(),
            line(),
        )
    }

    fn import(
        &self,
        i: &'a Import<EcoString>,
    ) -> (Vec<Document<'a>>, Vec<Document<'a>>, Vec<Document<'a>>) {
        let Import {
            module,
            as_name,
            unqualified_values,
            unqualified_types,
            package,
            ..
        } = i;

        let mut open_statements = Vec::new();
        let mut module_aliases = Vec::new();
        let mut other_aliases = Vec::new();
        let full_module_name = self.sanitize_str(module);

        match as_name {
            Some((AssignName::Variable(name), _)) => {
                module_aliases.push(docvec![
                    "module ",
                    self.sanitize_name(name),
                    " = ",
                    &full_module_name
                ]);
            }
            Some((AssignName::Discard(_), _)) => {}
            // If the imports are from the same package, don't need to open it
            None if unqualified_values.is_empty() && unqualified_types.is_empty() => {
                // if package is empty, it's from a builtin
                if package.is_empty() || package == "gleam" || package == "gleam_dotnet_stdlib" {
                    open_statements.push(docvec!["open ", &full_module_name]);
                } else if self.package_name == package {
                } else {
                    open_statements.push(docvec!["open ", self.sanitize_name(package)]);
                }
            }
            None => {}
        };

        unqualified_values.iter().for_each(|v| {
            let name = &v.name;
            let label = v.as_name.as_ref().unwrap_or(name);
            other_aliases.push(docvec![
                "let ",
                self.sanitize_name(label),
                " = ",
                &full_module_name,
                ".",
                self.sanitize_name(name)
            ]);
        });
        // unqualified_types.iter().for_each(|v| {
        //     let name = &v.name;
        //     let label = v.as_name.as_ref().unwrap_or(name);
        //     other_aliases.push(docvec![
        //         "let ",
        //         self.sanitize_name(label),
        //         " = ",
        //         &full_module_name,
        //         ".",
        //         self.sanitize_name(name)
        //     ]);
        // });

        // let mut values = Vec::new();
        // values.push(module_ref);
        // values.extend(open_statements);
        // values.extend(other_aliases);
        // values.extend(module_aliases);

        // join(values, line())

        (open_statements, module_aliases, other_aliases)
    }

    fn map_publicity(&self, publicity: &'a Publicity) -> Document<'a> {
        match publicity {
            Publicity::Public => nil(),
            Publicity::Internal { .. } => "internal ".to_doc(),
            Publicity::Private => "private ".to_doc(),
        }
    }

    fn is_fsharp_union_type(&self, t: &'a CustomType<Arc<Type>>) -> bool {
        // If there is more than one type constructor, it must be an F# union type
        if t.constructors.len() != 1 {
            true
        } else {
            assert!(t.constructors.len() == 1);
            // Might be an F# record type, but only if the constructor name
            // matches the type name and all constructor arguments have a label
            let c = t
                .constructors
                .first()
                .expect("Type must have a constructor");

            // Single constructor must match the type name and all arguments must have labels, otherwise
            // it must be an F# union type
            if c.name != t.name
                || c.arguments.is_empty()
                || c.arguments.iter().any(|a| a.label.is_none())
            {
                return true;
            }
            false
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
    fn custom_type(&self, type_: &'a CustomType<Arc<Type>>) -> Document<'a> {
        if type_.constructors.is_empty() {
            self.external_type(type_)
        } else if self.is_fsharp_union_type(type_) {
            self.discriminated_union_type(type_)
        } else {
            self.record_type(type_)
        }
    }

    /// TODO: Is this safe or incorrect?
    /// We should probably emit a warning at least
    fn external_type(&self, type_: &'a CustomType<Arc<Type>>) -> Document<'a> {
        return nil();
        // let name = &type_.name;
        // let values = if !type_.typed_parameters.is_empty() {
        //     join(
        //         type_
        //             .typed_parameters
        //             .iter()
        //             .map(|tp| self.type_to_fsharp(tp)),
        //         " * ".to_doc(),
        //     )
        // } else {
        //     nil()
        // };
        // docvec![
        //     "/// NOTE: This type has no clear definition so the compiler made a best guess at a way to represent it.", line(),
        //     "/// Please report any issues", line(),
        //     self.documentation(&type_.documentation),
        //     self.deprecation(&type_.deprecation),
        //     "type ",
        //     self.map_publicity(&type_.publicity),
        //     name,
        //     self.type_params(type_),
        //     " = ",
        //     name,
        //     " of ",
        //     values
        // ]
    }

    fn record_type(&self, type_: &'a CustomType<Arc<Type>>) -> Document<'a> {
        let constructor = type_
            .constructors
            .first()
            .expect("Single constructor should exist");

        let name = &type_.name;
        let fields = constructor
            .arguments
            .iter()
            .map(|r| {
                let type_doc = self.type_to_fsharp(&r.type_);
                match &r.label {
                    Some((_, ref label)) => docvec![self.sanitize_name(label), ": ", type_doc],
                    None => type_doc,
                }
            })
            .collect::<Vec<Document<'a>>>();

        let opacity = if type_.opaque {
            line()
                .nest(INDENT)
                .append("private ".to_doc())
                .append(line().nest(INDENT * 2))
        } else {
            nil()
        };
        docvec![
            self.documentation(&type_.documentation),
            self.deprecation(&type_.deprecation),
            "type ",
            self.map_publicity(&type_.publicity),
            name,
            self.type_params(type_),
            " = ",
            opacity,
            join(fields, "; ".to_doc())
                .surround("{ ", " }")
                .group()
                .nest(INDENT),
        ]
    }

    fn type_params(&self, t: &CustomType<Arc<Type>>) -> Document<'a> {
        if !t.typed_parameters.is_empty() {
            join(
                t.typed_parameters.iter().map(|tp| self.type_to_fsharp(tp)),
                ", ".to_doc(),
            )
            .surround("<", ">")
        } else {
            nil()
        }
    }

    fn discriminated_union_type(&self, t: &'a CustomType<Arc<Type>>) -> Document<'a> {
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
                        let type_ = self.type_to_fsharp(&r.type_);
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

        let opacity = if t.opaque {
            line()
                .nest(INDENT)
                .append("private ".to_doc())
                .append(line().nest(INDENT))
        } else {
            line()
        };

        let case_indent = if t.opaque { INDENT } else { 0 };

        docvec![
            "type ",
            self.map_publicity(&t.publicity),
            type_name,
            self.type_params(t),
            " =",
            opacity,
            join(
                constructors.into_iter().map(|c| docvec!["| ".to_doc(), c]),
                line()
            )
            .group()
            .nest(case_indent),
            line().nest(INDENT),
            member_declarations_doc
        ]
    }

    fn documentation(&self, documentation: &'a Option<(u32, EcoString)>) -> Document<'a> {
        match documentation {
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
        }
    }

    fn deprecation(&self, deprecation: &'a Deprecation) -> Document<'a> {
        match deprecation {
            Deprecation::Deprecated { message } => {
                docvec!["[<System.Obsolete(", self.string(message), ")>]", line()]
            }
            Deprecation::NotDeprecated => nil(),
        }
    }

    fn type_alias(&self, t: &'a TypeAlias<Arc<Type>>) -> Document<'a> {
        let TypeAlias {
            alias,
            type_,
            publicity,
            documentation,
            deprecation,
            // parameters,
            // type_ast,
            ..
        } = t;

        docvec![
            self.documentation(documentation),
            self.deprecation(deprecation),
            "type ",
            self.map_publicity(publicity),
            alias.clone(),
            " = ",
            self.type_to_fsharp(type_)
        ]
    }

    fn function(&mut self, f: &'a TypedFunction) -> Document<'a> {
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
        let name_str = name.as_ref().map(|n| n.1.as_str()).unwrap_or("_");

        let sanitized_name = self.sanitize_str(name_str);

        let body = match external_fsharp {
            Some((ref module_name, ref fn_name, _)) => {
                // TODO: look into tracking references to the external function in Call expressions
                // and call them directly instead? Or maybe add an inline specifier for this [instead]?

                let calling_args = if arguments.is_empty() {
                    "()".to_doc()
                } else {
                    join(arguments.iter().map(|arg| self.arg_name(arg)), " ".to_doc())
                };

                // If the "module" qualifier is a file path, assume that fn_name is fully qualified
                let qualifier = if module_name.contains("/") || module_name.contains("\\") {
                    let full_path = self
                        .input_file_path
                        .parent()
                        .expect("must have a parent")
                        .join(module_name.as_str());
                    _ =
                        self.external_files
                            .insert(full_path.canonicalize_utf8().unwrap_or_else(|_| {
                                panic!("Failed to canonicalize path: {full_path}")
                            }));
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
                        .append(self.statements(body, Some(return_type)))
                        .nest(INDENT)
                        .group(),
                    line(),
                    "end"
                ]
            }
        };

        let args = self.fun_args(arguments);
        let docs = self.documentation(documentation);

        let deprecation_doc = self.deprecation(deprecation);

        // TODO: Make this less magic
        let (entry_point_annotation, args) = if name_str == "main" {
            match &arguments[..] {
                [TypedArg {
                    names: ArgNames::Named { name, .. },
                    ..
                }] => (
                    "[<EntryPoint>]".to_doc().append(line()),
                    EcoString::from(format!("({}: string[])", name)).to_doc(),
                ),
                []
                | [TypedArg {
                    names: ArgNames::Discard { .. },
                    ..
                }] => (
                    "[<EntryPoint>]".to_doc().append(line()),
                    "(_: string[])".to_doc(),
                ),
                _ => (nil(), args),
            }
        } else {
            (nil(), args)
        };

        // For now, since we mark all modules as recursive, we don't need to mark
        // functions as recursive.
        docvec![
            docs,
            deprecation_doc,
            entry_point_annotation,
            "let ",
            self.map_publicity(&f.publicity),
            sanitized_name,
            " ",
            args,
            // ": ",
            // return_type,
            " = ",
            body
        ]
    }

    fn arg_name(&self, arg: &'a TypedArg) -> Document<'a> {
        arg.names
            .get_variable_name()
            .map(|n| self.sanitize_name(n).to_doc())
            .unwrap_or_else(|| "_".to_doc())
    }

    /// Function definition arguments
    fn fun_args(&self, arguments: &'a [TypedArg]) -> Document<'a> {
        if arguments.is_empty() {
            "()".to_doc()
        } else {
            join(
                arguments.iter().map(|arg| {
                    docvec![self.arg_name(arg), ": ", self.type_to_fsharp(&arg.type_)]
                        .surround("(", ")")
                }),
                " ".to_doc(),
            )
            .group()
        }
    }

    fn body_must_be_multiline(&self, body: &'a [TypedStatement]) -> bool {
        if body.len() > 1 {
            return true;
        }
        body.iter().any(|s| match s {
            Statement::Expression(e) => self.must_be_multiline(e),
            _ => true,
        })
    }

    /// Anonymous functions
    fn fun(&self, args: &'a [TypedArg], body: &'a Vec1<TypedStatement>) -> Document<'a> {
        if !self.body_must_be_multiline(body) {
            return docvec![
                "fun",
                self.fun_args(args),
                " -> ",
                self.statement(body.first()).0
            ];
        }

        docvec![
            "fun",
            self.fun_args(args),
            " -> begin",
            line()
                .nest(INDENT)
                .append(self.statements(body, None).nest(INDENT).append(line()))
                .append("end"),
        ]
    }
    // /// Anonymous functions
    // fn fun(&self, args: &'a [TypedArg], body: &'a [TypedStatement]) -> Document<'a> {
    //     let output = docvec![
    //         "fun",
    //         self.fun_args(args),
    //         " ->",
    //         break_(" begin\n", " ")
    //             .nest_if_broken(INDENT)
    //             .append(
    //                 self.statements(body, None)
    //                     .nest_if_broken(INDENT)
    //                     //.append(line())
    //             )
    //             .append(break_("\nend", "")),
    //     ];

    //     if self.body_must_be_multiline(body) {
    //         output.force_break() //.nest(INDENT)
    //     } else {
    //         output
    //     }
    // }

    fn statement(&self, s: &'a TypedStatement) -> (Document<'a>, Option<Document<'a>>) {
        let mut last_var = None;
        let statement_doc = match s {
            Statement::Expression(expr) => {
                last_var = None;
                self.expression(expr)
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
                let suffix_binding_name: Document<'a> = match right_side_assignment {
                    AssignName::Variable(right) => {
                        let v = right.to_doc();

                        last_var = Some(v.clone());
                        v
                    }
                    AssignName::Discard(_) => {
                        last_var = Some(self.expression(value).to_doc());
                        "_".to_doc()
                    }
                };

                docvec![
                    "let (",
                    prelude_functions::STRING_PATTERN_PARTS,
                    " ",
                    self.string(prefix),
                    " (",
                    match maybe_label {
                        Some((prefix_label, _)) => docvec![prefix_label, ", "],
                        None => "_, ".to_doc(),
                    },
                    suffix_binding_name,
                    ")) = ",
                    self.expression(value).to_doc(),
                ]
            }

            Statement::Assignment(a) => {
                let (name, can_use_as_return_value) = self.get_assignment_binding(&a.pattern);

                if can_use_as_return_value {
                    last_var = Some(name.clone());
                }
                self.assignment(name, &a.value)

            }
            Statement::Use(_) => docvec!["// This should never be emitted, use statements are transformed into function calls"],
        };

        (statement_doc, last_var)
    }

    fn get_assignment_binding(&self, pattern: &'a TypedPattern) -> (Document<'a>, bool) {
        let name = self.pattern(pattern);
        // Need to disallow a case where the matches in a record constructor pattern only include discards
        (name, true)
        // match pattern {
        //     TypedPattern::Int { .. } | TypedPattern::Float { .. } | TypedPattern::String { .. } => {
        //         (name, true)
        //     }
        //     TypedPattern::Variable { .. } => (name, true),
        //     TypedPattern::Assign { name, .. } => (name.to_doc(), true),
        //     TypedPattern::Constructor { type_, .. } if type_.is_bool() => (name.to_doc(), true),
        //     // Need to disallow a case where the matches in a record constructor pattern only include discards
        //     TypedPattern::Constructor { constructor, .. } => {
        //         println!("constructor: {:?}", constructor);
        //         (name, false)
        //     }
        //     _ => (name, true),
        // }
    }

    fn assignment(&self, name: Document<'a>, value: &'a TypedExpr) -> Document<'a> {
        let value_doc = self.expression(value);

        if self.must_start_assignment_value_on_newline(value) {
            docvec![
                "let ",
                name,
                " =",
                line().nest(INDENT).append(value_doc.nest(INDENT))
            ]
        } else {
            docvec!["let ", name, " = ", value_doc]
        }
    }

    fn must_start_assignment_value_on_newline(&self, value: &'a TypedExpr) -> bool {
        if matches!(value, TypedExpr::Case { .. } | TypedExpr::Fn { .. }) || self.is_iife(value) {
            return true;
        }

        if let TypedExpr::Call { args, .. } = value {
            return self.any_arg_must_be_multiline(args);
        }

        false
    }

    /// If the expression is an immediately-invoked function expression (IIFE)
    fn is_iife(&self, expr: &'a TypedExpr) -> bool {
        if let TypedExpr::Call { fun, .. } = expr {
            if let TypedExpr::Fn { body, .. } = fun.as_ref() {
                if body.len() == 1 {
                    if let Statement::Expression(TypedExpr::Call { fun: inner_fun, .. }) =
                        body.first()
                    {
                        if let TypedExpr::Fn { .. } = inner_fun.as_ref() {
                            return true;
                        }
                        return true;
                    }
                }
            }
        }
        false
    }

    fn statements(&self, s: &'a [TypedStatement], return_type: Option<&Type>) -> Document<'a> {
        let mut last_var = None;
        let mut res = s
            .iter()
            .map(|s| {
                let statement_info = self.statement(s);
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

        // match (return_type, last_var) {
        //     (None, None) => {}
        //     (None, Some(last_var)) => res.push(last_var),

        //     (Some(return_type), None) => {
        //         // TODO: If the last statement is a let assert, we need to either figure out the right value to return, or disallow it

        //         // if !return_type.is_nil() {
        //         //     // TODO: Fix this somehow?
        //         //     res.push(
        //         //         "failwith \"An error occurred while compiling the previous statements: couldn't figure out what to return\""
        //         //             .to_doc(),
        //         //     );
        //         // } else {
        //         //     res.push("()".to_doc());
        //         // }
        //     }
        //     (Some(return_type), Some(last_var)) => {
        //         if !return_type.is_nil() {
        //             res.push(last_var);
        //         }
        //         // else {
        //         //     res.push(last_var)
        //         // };
        //     }
        // }

        join(res, line())
    }

    fn sanitize_str(&self, value: &'a str) -> EcoString {
        let mapped = value
            .split("/")
            .map(|s| {
                if is_reserved_word(s) {
                    format!("``{s}``")
                } else {
                    String::from(s)
                }
            })
            .join(".");

        EcoString::from(mapped)
    }

    fn sanitize_name(&self, name: &'a EcoString) -> Document<'a> {
        self.sanitize_str(name.as_str()).to_doc()
    }
    fn string_inner(&self, value: &str) -> Document<'a> {
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
        EcoString::from(content).to_doc()
    }

    fn string(&self, value: &str) -> Document<'a> {
        self.string_inner(value).surround("\"", "\"")
    }

    /// When running on the Erlang virtual machine ints have no maximum and minimum size.
    /// When running on JavaScript runtimes ints are represented using JavaScript's 64 bit floating point numbers,
    /// For now we'll assume for maximum compatibility that we'll need to use 64 bits for ints
    fn integer(&self, value: &'a EcoString) -> Document<'a> {
        value.to_doc().append("L")
    }

    fn expression(&self, expr: &'a TypedExpr) -> Document<'a> {
        match expr {
            TypedExpr::Int { value, .. } => self.integer(value),
            TypedExpr::Float { value, .. } => value.to_doc(),
            TypedExpr::String { value, .. } => self.string(value.as_str()),
            TypedExpr::Block { statements, .. } => self.block(statements),
            TypedExpr::Pipeline {
                assignments,
                finally,
                ..
            } => self.pipeline(assignments, finally),

            TypedExpr::Var { name, .. } => match name.as_str() {
                "Nil" => "()".to_doc(),
                "True" => "true".to_doc(),
                "False" => "false".to_doc(),
                _ => self.sanitize_name(name).to_doc(),
            },
            TypedExpr::Fn { args, body, .. } => self.fun(args, body),
            TypedExpr::List { elements, tail, .. } => {
                let list = join(elements.iter().map(|e| self.expression(e)), "; ".to_doc())
                    .surround("[", "]");

                match tail {
                    Some(tail) => docvec![list, " @ ", self.expression(tail)],
                    None => list,
                }
            }

            TypedExpr::Call { fun, args, .. } => match fun.as_ref() {
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::Record {
                                    constructors_count: 1,
                                    arity,
                                    field_map: Some(ref field_map),
                                    ..
                                },
                            ..
                        },
                    ..
                } if *arity == field_map.fields.len() as u16 => {
                    // Every constructor field must have a label to be a record type
                    self.record_instantiation(field_map, args)
                }
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::Record { .. },
                            ..
                        },
                    ..
                } => self.function_call(false, fun, args),
                _ => self.function_call(true, fun, args),
            },

            TypedExpr::BinOp {
                left, right, name, ..
            } => self.binop(name, left, right),

            TypedExpr::Case {
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            TypedExpr::Tuple { elems, .. } => {
                let items = elems.iter().map(|e| self.expression(e)).collect_vec();
                self.tuple(items)
            }

            // Special case for double negation
            TypedExpr::NegateInt { value, .. } => {
                // Special case for double negation
                if let TypedExpr::NegateInt { .. } = value.as_ref() {
                    "-".to_doc()
                        .append(self.expression(value).surround("(", ")"))
                } else {
                    "-".to_doc().append(self.expression(value))
                }
            }

            TypedExpr::Todo { message, .. } => self.todo(message),
            TypedExpr::Panic { message, .. } => self.panic_(message),
            TypedExpr::RecordAccess { label, record, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate { args, spread, .. } => {
                // If the target of the update is the result of a pipeline, it needs to be
                // surrounded in parentheses
                let old_var_name = match spread.deref() {
                    TypedExpr::Pipeline { .. } => self.expression(spread).surround("(", ")"),
                    _ => self.expression(spread),
                };

                let new_values = args.iter().map(|arg| {
                    let child_expr = match &arg.value {
                        // If the child here is a pipe operation, we need to indent at least
                        // one more space so that it starts on a column after the `with` keyword
                        TypedExpr::Pipeline { .. } => self.expression(&arg.value).nest(1),
                        _ => self.expression(&arg.value),
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
            TypedExpr::ModuleSelect {
                module_name, label, ..
            } => {
                let full_module_name = self.sanitize_name(module_name);
                let full_module_name = if full_module_name.is_empty() {
                    self.sanitize_name(label).to_doc()
                } else {
                    docvec![full_module_name, ".", self.sanitize_name(label)]
                };
                full_module_name
            }
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, index),
            TypedExpr::NegateBool { value, .. } => {
                docvec!["not ", self.expression(value).surround("(", ")")]
            }
            TypedExpr::BitArray { .. } => "BitArray".to_doc(),
            TypedExpr::Invalid { .. } => "// TODO: TypedExpr::Invalid".to_doc(),
        }
    }

    fn tuple_index(&self, tuple: &'a TypedExpr, index: &'a u64) -> Document<'a> {
        // TODO: Add warning suppression when this is encountered:
        // #nowarn "3220" // This method or property is not normally used from F# code, use an explicit tuple pattern for deconstruction instead.
        docvec![self.expression(tuple), ".Item", index + 1]
    }

    // fn union_instantiation(&self, args: &'a [CallArg<TypedExpr>]) -> Document<'a> {
    //     let args = args.iter().enumerate().map(|(i, arg)| {
    //         let label = field_map.get(&(i as u32)).expect("Index out of bounds");
    //         docvec![
    //             self.sanitize_name(label).to_doc(),
    //             " = ",
    //             self.expression(&arg.value)
    //         ]
    //     });
    // }

    // fn constant_union_constructor(
    //     &self,
    //     type_: &'a Arc<Type>,
    //     record_name: &'a EcoString,
    //     module: &'a Option<(EcoString, SrcSpan)>,
    //     field_map: &'a Option<FieldMap>,
    //     args: &'a [CallArg<TypedConstant>],
    // ) -> Document<'a> {
    //     // If there's no arguments and the type is a function that takes
    //     // arguments then this is the constructor being referenced, not the
    //     // function being called.
    //     if let Some(arity) = type_.fn_arity() {
    //         if type_.is_bool() && record_name == "True" {
    //             return "true".to_doc();
    //         } else if type_.is_bool() {
    //             return "false".to_doc();
    //         } else if type_.is_nil() {
    //             return "undefined".to_doc();
    //         } else if arity == 0 {
    //             return match module {
    //                 Some((module, _)) => docvec![module, ".", record_name, "()"],
    //                 None => docvec![record_name, "()"],
    //             };
    //         } else if let Some((module, _)) = module {
    //             return docvec![module, ".", self.sanitize_name(record_name)];
    //         } else {
    //             return self.sanitize_name(record_name).to_doc();
    //         }
    //     }

    //     if field_map.is_none() && args.is_empty() {
    //         return self.sanitize_name(record_name).to_doc();
    //     }

    //     let field_values: Vec<_> = args
    //         .iter()
    //         .map(|arg| self.constant_expression(&arg.value))
    //         .collect();

    //     self.construct_type(
    //         module.as_ref().map(|(module, _)| module.as_str()),
    //         record_name,
    //         field_values,
    //     )
    // }

    fn record_instantiation(
        &self,
        field_map: &'a FieldMap,
        args: &'a [CallArg<TypedExpr>],
    ) -> Document<'a> {
        let field_map = invert_field_map(field_map);

        let args = args.iter().enumerate().map(|(i, arg)| {
            let label = field_map.get(&(i as u32)).expect("Index out of bounds");
            docvec![
                self.sanitize_name(label).to_doc(),
                " = ",
                self.expression(&arg.value)
            ]
        });

        join(args, "; ".to_doc()).group().surround("{ ", " }")
    }

    // If an expression is one of these types, it must take up multiple lines, regardless of how long it is
    fn must_be_multiline(&self, expr: &'a TypedExpr) -> bool {
        if matches!(
            expr,
            TypedExpr::Pipeline { .. } | TypedExpr::Fn { .. } | TypedExpr::Case { .. }
        ) {
            return true;
        }

        if let TypedExpr::Call { args, .. } = expr {
            return self.any_arg_must_be_multiline(args);
        }

        if let TypedExpr::List { elements, .. } = expr {
            return elements.iter().any(|e| self.must_be_multiline(e));
        }
        if let TypedExpr::Tuple { elems, .. } = expr {
            return elems.iter().any(|e| self.must_be_multiline(e));
        }

        false
    }

    fn any_arg_must_be_multiline(&self, args: &'a [TypedCallArg]) -> bool {
        args.iter().any(|arg| self.must_be_multiline(&arg.value))
    }

    fn function_call(
        &self,
        curried: bool,
        fun: &'a TypedExpr,
        args: &'a [TypedCallArg],
    ) -> Document<'a> {
        let must_be_multiline = self.any_arg_must_be_multiline(args);

        let args = if args.is_empty() {
            "()".to_doc()
        } else {
            let start: Document<'_> = if must_be_multiline {
                line()
            } else if curried {
                " ".to_doc()
            } else {
                nil()
            };

            let sep = if !curried {
                ", ".to_doc().append(start.clone())
            } else {
                start.clone()
            };

            let args = args
                .iter()
                .map(|a| self.expression(&a.value).surround("(", ")"));

            let args = start.append(join(args, sep)).nest(INDENT).group();

            if curried {
                args
            } else {
                args.surround("(", ")")
            }
        };
        // If for some reason we're doing an IIFE, we need to wrap it in parens
        let fun_expr = if matches!(fun, TypedExpr::Fn { .. }) {
            self.expression(fun).surround("(", ")")
        } else {
            self.expression(fun)
        };
        fun_expr.append(args).group()
    }

    fn record_access(&self, record: &'a TypedExpr, label: &'a EcoString) -> Document<'a> {
        let record_expr = self.expression(record);
        docvec![record_expr, ".", self.sanitize_name(label)]
    }

    fn todo(&self, message: &'a Option<Box<TypedExpr>>) -> Document<'a> {
        match message {
            Some(message) => "failwith "
                .to_doc()
                .append(self.expression(message.as_ref()).surround("(", ")")),
            None => "failwith \"Not implemented\"".to_doc(),
        }
    }

    fn panic_(&self, message: &'a Option<Box<TypedExpr>>) -> Document<'a> {
        match message {
            Some(message) => "failwith "
                .to_doc()
                .append(self.expression(message.as_ref()).surround("(", ")")),
            None => "failwith \"Panic encountered\"".to_doc(),
        }
    }

    fn tuple(&self, elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
        join(elements, ", ".to_doc()).surround("(", ")")
    }

    fn case(&self, subjects: &'a [TypedExpr], clauses: &'a [TypedClause]) -> Document<'a> {
        let subjects_doc = if subjects.len() == 1 {
            self.expression(
                subjects
                    .first()
                    .expect("f# case printing of single subject"),
            )
        } else {
            let items = subjects.iter().map(|s| self.expression(s)).collect_vec();
            self.tuple(items)
        };

        let clauses = join(
            clauses.iter().map(|c| docvec!["| ", self.clause(c)]),
            line(),
        );

        docvec![
            break_(" ", ""),
            docvec!["match ", subjects_doc, " with"],
            break_(" ", ""),
            line().append(clauses),
        ]
        .group()
    }

    fn clause(&self, clause: &'a TypedClause) -> Document<'a> {
        let Clause {
            guard,
            pattern: pat,
            alternative_patterns,
            then,
            ..
        } = clause;

        let additional_guards = vec![];
        let patterns_doc = join(
            std::iter::once(pat)
                .chain(alternative_patterns)
                .map(|patterns| {
                    let patterns_doc = if patterns.len() == 1 {
                        let p = patterns.first().expect("Single pattern clause printing");
                        self.pattern(p)
                    } else {
                        let items = patterns.iter().map(|p| self.pattern(p)).collect_vec();
                        self.tuple(items)
                    };

                    patterns_doc
                }),
            " | ".to_doc(),
        );
        let guard = self.optional_clause_guard(guard.as_ref(), additional_guards);
        docvec![
            patterns_doc,
            guard,
            " ->",
            line()
                .nest(INDENT)
                .append(self.clause_consequence(then).nest(INDENT))
        ]
    }

    fn clause_consequence(&self, consequence: &'a TypedExpr) -> Document<'a> {
        match consequence {
            TypedExpr::Block { statements, .. } => self.statement_sequence(statements),
            _ => self.expression(consequence),
        }
    }

    fn statement_sequence(&self, statements: &'a [TypedStatement]) -> Document<'a> {
        let documents = statements.iter().map(|e| self.statement(e).0.group());

        let documents = join(documents, line());
        if statements.len() == 1 {
            documents
        } else {
            documents.force_break()
        }
    }
    fn optional_clause_guard(
        &self,
        guard: Option<&'a TypedClauseGuard>,
        additional_guards: Vec<Document<'a>>,
    ) -> Document<'a> {
        let guard_doc = guard.map(|guard| self.bare_clause_guard(guard));

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
    fn clause_guard(&self, guard: &'a TypedClauseGuard) -> Document<'a> {
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
            | ClauseGuard::RemainderInt { .. } => "("
                .to_doc()
                .append(self.bare_clause_guard(guard))
                .append(")"),

            // Other expressions are not
            ClauseGuard::Constant(_)
            | ClauseGuard::Not { .. }
            | ClauseGuard::Var { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::ModuleSelect { .. } => self.bare_clause_guard(guard),
        }
    }
    fn bare_clause_guard(&self, guard: &'a TypedClauseGuard) -> Document<'a> {
        match guard {
            ClauseGuard::Not { expression, .. } => {
                docvec!["not ", self.bare_clause_guard(expression)]
            }

            ClauseGuard::Or { left, right, .. } => self
                .clause_guard(left)
                .append(" || ")
                .append(self.clause_guard(right)),

            ClauseGuard::And { left, right, .. } => self
                .clause_guard(left)
                .append(" && ")
                .append(self.clause_guard(right)),

            ClauseGuard::Equals { left, right, .. } => self
                .clause_guard(left)
                .append(" = ")
                .append(self.clause_guard(right)),

            ClauseGuard::NotEquals { left, right, .. } => self
                .clause_guard(left)
                .append(" <> ")
                .append(self.clause_guard(right)),

            ClauseGuard::GtInt { left, right, .. } | ClauseGuard::GtFloat { left, right, .. } => {
                self.clause_guard(left)
                    .append(" > ")
                    .append(self.clause_guard(right))
            }

            ClauseGuard::GtEqInt { left, right, .. }
            | ClauseGuard::GtEqFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" >= ")
                .append(self.clause_guard(right)),

            ClauseGuard::LtInt { left, right, .. } | ClauseGuard::LtFloat { left, right, .. } => {
                self.clause_guard(left)
                    .append(" < ")
                    .append(self.clause_guard(right))
            }

            ClauseGuard::LtEqInt { left, right, .. }
            | ClauseGuard::LtEqFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" <= ")
                .append(self.clause_guard(right)),

            ClauseGuard::AddInt { left, right, .. } | ClauseGuard::AddFloat { left, right, .. } => {
                self.clause_guard(left)
                    .append(" + ")
                    .append(self.clause_guard(right))
            }

            ClauseGuard::SubInt { left, right, .. } | ClauseGuard::SubFloat { left, right, .. } => {
                self.clause_guard(left)
                    .append(" - ")
                    .append(self.clause_guard(right))
            }

            ClauseGuard::MultInt { left, right, .. }
            | ClauseGuard::MultFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" * ")
                .append(self.clause_guard(right)),

            ClauseGuard::DivInt { left, right, .. } | ClauseGuard::DivFloat { left, right, .. } => {
                self.clause_guard(left)
                    .append(" / ")
                    .append(self.clause_guard(right))
            }

            ClauseGuard::RemainderInt { left, right, .. } => self
                .clause_guard(left)
                .append(" % ")
                .append(self.clause_guard(right)),

            // ClauseGuard::Vars are local variables
            ClauseGuard::Var { name, .. } => self.sanitize_name(name).to_doc(),

            ClauseGuard::Constant(c) => self.constant_expression(c),
            ClauseGuard::TupleIndex { tuple, index, .. } => {
                // TODO: Add warning suppression when this is encountered:
                // #nowarn "3220" // This method or property is not normally used from F# code, use an explicit tuple pattern for deconstruction instead.
                docvec![self.clause_guard(tuple), ".Item", index + 1]
            }
            ClauseGuard::FieldAccess {
                container, label, ..
            } => self
                .clause_guard(container)
                .append(".")
                .append(self.sanitize_name(label)),
            ClauseGuard::ModuleSelect {
                module_alias,
                label,
                ..
            } => {
                docvec![module_alias, ".", label]
            }
        }
    }

    fn binop(&self, name: &'a BinOp, left: &'a TypedExpr, right: &'a TypedExpr) -> Document<'a> {
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

    /// Implement pipeline (|>) expressions
    fn pipeline(&self, assignments: &'a [TypedAssignment], finally: &'a TypedExpr) -> Document<'a> {
        let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

        for a in assignments {
            let name = self.pattern(&a.pattern);
            let assignment = self.assignment(name, &a.value);
            documents.push(assignment);
            documents.push(line());
        }

        documents.push(self.expression(finally).surround("(", ")"));

        self.wrap_in_begin_end(documents.to_doc())
    }

    fn wrap_in_begin_end(&self, expr: Document<'a>) -> Document<'a> {
        "begin"
            .to_doc()
            .append(line())
            .nest(INDENT)
            .append(expr.nest(INDENT).group())
            .append(line().append("end"))
    }

    fn block(&self, s: &'a [TypedStatement]) -> Document<'a> {
        "begin"
            .to_doc()
            .append(line())
            .nest(INDENT)
            .append(self.statements(s, None).nest(INDENT).group())
            .append(line().append("end"))
    }

    fn pattern(&self, p: &'a Pattern<Arc<Type>>) -> Document<'a> {
        match p {
            Pattern::Int { value, .. } => self.integer(value),
            Pattern::Float { value, .. } => value.to_doc(),
            Pattern::String { value, .. } => self.string(value.as_str()),
            Pattern::Variable { name, .. } => self.sanitize_name(name).to_doc(),
            Pattern::Discard { name, .. } => name.to_doc(),
            Pattern::List { elements, tail, .. } => {
                // let elements_doc = join(elements.iter().map(|e| self.pattern(e)), "; ".to_doc());
                // let head = if elements.len() == 1 {
                //     elements_doc
                // } else {
                //     elements_doc.surround("[", "]")
                // };
                match tail {
                    Some(tail) => {
                        let items = if elements.is_empty() {
                            "[]".to_doc()
                        } else {
                            join(elements.iter().map(|e| self.pattern(e)), "::".to_doc())
                        };
                        items.append("::").append(self.pattern(tail))
                    }
                    None => join(elements.iter().map(|e| self.pattern(e)), "; ".to_doc())
                        .surround("[", "]"),
                }
            }
            Pattern::Tuple { elems, .. } => {
                join(elems.iter().map(|e| self.pattern(e)), ", ".to_doc()).surround("(", ")")
            }

            Pattern::StringPrefix {
                left_side_string: prefix,
                right_side_assignment,
                left_side_assignment: maybe_prefix_label,
                ..
            } => {
                // TODO: Add warning suppression when this is encountered:
                // #nowarn "25" // Incomplete pattern matches on this expression.
                let suffix_binding_name: Document<'a> = match right_side_assignment {
                    AssignName::Variable(right) => right.to_doc(),
                    AssignName::Discard(_) => "_".to_doc(),
                };

                match maybe_prefix_label {
                    None => {
                        docvec![
                            prelude_functions::STRING_PATTERN_PREFIX,
                            " ",
                            self.string(prefix),
                            " ",
                            suffix_binding_name,
                        ]
                    }
                    Some((prefix_label, _)) => {
                        docvec![
                            prelude_functions::STRING_PATTERN_PARTS,
                            " ",
                            self.string(prefix),
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
                let segments_docs = segments.iter().map(|s| self.pattern(&s.value));
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
                        self.constant_expression(literal)
                    }
                    _ => name.to_doc(),
                }
            }
            Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
            Pattern::Assign {
                name, pattern: p, ..
            } => self.pattern(p).append(" as ").append(name),

            Pattern::Constructor {
                constructor:
                    pattern_constructor @ Inferred::Known(PatternConstructor {
                        name,
                        field_map: Some(ref field_map),
                        ..
                    }),
                spread,
                arguments,
                type_,
                ..
            } if arguments.len() == field_map.fields.len() => {
                let (_, type_name) = type_.named_type_name().expect("Expected a named type");
                let constructor = self
                    .module
                    .type_info
                    .types_value_constructors
                    .get(&type_name);

                // If there's more than one possible constructor, we can't print this like a record type
                if let Some(c) = constructor {
                    if c.variants.len() > 1 {
                        return self.constructor_pattern(arguments, pattern_constructor, name);
                    }
                }

                let field_map = invert_field_map(field_map);

                let args = arguments.iter().enumerate().filter_map(|(i, arg)| {
                    if spread.is_some() && arg.value.is_discard() {
                        return None;
                    }

                    let label = match &arg.label {
                        Some(label) => Some(self.sanitize_name(label)),
                        None => field_map
                            .get(&(i as u32))
                            .map(|label| self.sanitize_name(label)),
                    };

                    label.map(|label| docvec![label, " = ", self.pattern(&arg.value)])
                });
                join(args, "; ".to_doc()).group().surround("{ ", " }")
            }

            Pattern::Constructor { name, type_, .. } if type_.is_bool() && name == "True" => {
                "true".to_doc()
            }
            Pattern::Constructor { name, type_, .. } if type_.is_bool() && name == "False" => {
                "false".to_doc()
            }

            Pattern::Constructor { type_, .. } if type_.is_nil() => "()".to_doc(),

            Pattern::Constructor {
                name,
                arguments,
                constructor,
                ..
            } => self.constructor_pattern(arguments, constructor, name),
        }
    }

    fn constructor_pattern(
        &self,
        arguments: &'a [CallArg<Pattern<Arc<Type>>>],
        constructor: &'a Inferred<PatternConstructor>,
        name: &'a EcoString,
    ) -> Document<'a> {
        let args = arguments
            .iter()
            .map(|arg| self.pattern(&arg.value))
            .collect_vec();
        let args = if arguments.is_empty() {
            if let Inferred::Known(PatternConstructor {
                field_map: None, ..
            }) = constructor
            {
                nil()
            } else {
                "()".to_doc()
            }
        } else {
            join(args, ", ".to_doc()).surround("(", ")")
        };
        docvec![name.to_doc(), args].surround("(", ")")
    }

    fn type_to_fsharp(&self, t: &Type) -> Document<'a> {
        if t.is_nil() {
            return "unit".to_doc();
        }

        match t {
            Type::Named { name, args, .. } => {
                let name = match name.as_str() {
                    "Int" | "int" => "int64".to_doc(),
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
                            args.iter().map(|arg| self.type_to_fsharp(arg)),
                            ", ".to_doc(),
                        ))
                        .append(">")
                }
            }
            Type::Fn { args, retrn, .. } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.type_to_fsharp(arg))
                    .collect::<Vec<Document<'a>>>();

                let arg_types = if arg_types.is_empty() {
                    "unit".to_doc()
                } else {
                    join(arg_types, " -> ".to_doc())
                };

                let return_type = self.type_to_fsharp(retrn);
                docvec![arg_types, " -> ", return_type]
            }
            Type::Tuple { elems } => {
                join(elems.iter().map(|t| self.type_to_fsharp(t)), " * ".to_doc())
                    .surround("(", ")")
            }
            Type::Var { type_ } => {
                let borrowed = type_.borrow();
                match borrowed.deref() {
                    TypeVar::Link { type_ } => self.type_to_fsharp(type_),
                    TypeVar::Unbound { id } => EcoString::from(format!("'u{}", id)).to_doc(),
                    TypeVar::Generic { id } => EcoString::from(format!("'t{}", id)).to_doc(),
                }
            }
        }
    }

    fn module_constant(&self, constant: &'a ModuleConstant<Arc<Type>, EcoString>) -> Document<'a> {
        let attr = match constant.value.deref() {
            Constant::Int { .. } | Constant::Float { .. } | Constant::String { .. } => {
                docvec!["[<Literal>]", line()]
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
                docvec!["[<Literal>]", line()]
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
                docvec!["[<Literal>]", line()]
            }

            _ => nil(),
        };
        docvec![
            attr,
            "let ",
            self.map_publicity(&constant.publicity),
            self.sanitize_name(&constant.name),
            " = ",
            self.constant_expression(&constant.value)
        ]
    }

    fn constant_expression(&self, expression: &'a TypedConstant) -> Document<'a> {
        match expression {
            Constant::Int { value, .. } => self.integer(value),
            Constant::Float { value, .. } => value.to_doc(),
            Constant::String { value, .. } => self.string(value),
            Constant::Tuple { elements, .. } => {
                let items = elements
                    .iter()
                    .map(|e| self.constant_expression(e))
                    .collect_vec();
                self.tuple(items)
            }

            Constant::List { elements, .. } => {
                let items = elements
                    .iter()
                    .map(|e| self.constant_expression(e))
                    .collect_vec();
                self.list(items)
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
                name: record_name,
                type_,
                field_map,
                ..
            } => self.constant_record_expression(record_name, args, type_, module, field_map),

            Constant::BitArray { .. } => "//TODO: Constant::BitArray".to_doc(),

            Constant::Var { name, module, .. } => {
                match module {
                    None => self.sanitize_name(name),
                    Some((module, _)) => {
                        // JS keywords can be accessed here, but we must escape anyway
                        // as we escape when exporting such names in the first place,
                        // and the imported name has to match the exported name.
                        docvec![module, ".", self.sanitize_name(name)]
                    }
                }
            }

            Constant::StringConcatenation { left, right, .. } => {
                let left = self.constant_expression(left);
                let right = self.constant_expression(right);
                docvec!(left, " + ", right)
            }

            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn constant_record_expression(
        &self,
        record_name: &'a EcoString,
        args: &'a [CallArg<TypedConstant>],
        type_: &'a Arc<Type>,
        module: &'a Option<(EcoString, SrcSpan)>,
        field_map: &'a Option<FieldMap>,
    ) -> Document<'a> {
        if let Some(constructor) = self.module.type_info.values.get(record_name) {
            if let ValueConstructorVariant::Record {
                name,
                constructors_count,
                field_map: Some(field_map),
                arity,
                ..
            } = &constructor.variant
            {
                // Is a genuine record constructor if:
                // only one constructor exists
                // constructor name is the same as the type name
                // All fields have labels

                if *constructors_count == 1u16
                    && name == record_name
                    && *arity == field_map.fields.len() as u16
                {
                    let field_map = invert_field_map(field_map);

                    let args = args.iter().enumerate().map(|(i, arg)| {
                        let label = field_map.get(&(i as u32)).expect("Index out of bounds");
                        docvec![label.to_doc(), " = ", self.constant_expression(&arg.value)]
                    });

                    return join(args, "; ".to_doc()).group().surround("{ ", " }");
                }
            }
        }

        self.constant_union_constructor(type_, record_name, module, field_map, args)
    }

    fn constant_union_constructor(
        &self,
        type_: &'a Arc<Type>,
        record_name: &'a EcoString,
        module: &'a Option<(EcoString, SrcSpan)>,
        field_map: &'a Option<FieldMap>,
        args: &'a [CallArg<TypedConstant>],
    ) -> Document<'a> {
        // If there's no arguments and the type is a function that takes
        // arguments then this is the constructor being referenced, not the
        // function being called.
        if let Some(arity) = type_.fn_arity() {
            if type_.is_bool() && record_name == "True" {
                return "true".to_doc();
            } else if type_.is_bool() {
                return "false".to_doc();
            } else if type_.is_nil() {
                return "undefined".to_doc();
            } else if arity == 0 {
                return match module {
                    Some((module, _)) => docvec![module, ".", record_name, "()"],
                    None => docvec![record_name, "()"],
                };
            } else if let Some((module, _)) = module {
                return docvec![module, ".", self.sanitize_name(record_name)];
            } else {
                return self.sanitize_name(record_name).to_doc();
            }
        }

        if field_map.is_none() && args.is_empty() {
            return self.sanitize_name(record_name).to_doc();
        }

        let field_values: Vec<_> = args
            .iter()
            .map(|arg| self.constant_expression(&arg.value))
            .collect();

        self.construct_type(
            module.as_ref().map(|(module, _)| module.as_str()),
            record_name,
            field_values,
        )
    }

    fn construct_type(
        &self,
        module: Option<&'a str>,
        name: &'a str,
        arguments: impl IntoIterator<Item = Document<'a>>,
    ) -> Document<'a> {
        let mut any_arguments = false;
        let arguments = join(
            arguments.into_iter().inspect(|_| {
                any_arguments = true;
            }),
            ", ".to_doc(),
        );
        let arguments = arguments.nest(INDENT);
        let name = if let Some(module) = module {
            docvec![module, ".", name]
        } else {
            name.to_doc()
        };
        if any_arguments {
            docvec![name, "(", arguments, ")"].group()
        } else {
            docvec![name, "()"]
        }
    }

    fn list(&self, elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
        join(elements, "; ".to_doc()).group().surround("[", "]")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
}
fn invert_field_map(field_map: &FieldMap) -> HashMap<&u32, &EcoString> {
    field_map
        .fields
        .iter()
        .map(|(k, v)| (v, k))
        .collect::<HashMap<_, _>>()
}
