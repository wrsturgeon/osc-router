//! Logic to turn a match-like router expression into a `char`-by-`char` parser.

use {
    proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree},
    std::collections::{BTreeMap, btree_map::Entry},
};

#[inline]
pub(crate) fn stratify(ts: TokenStream) -> Hierarchy {
    stratify_rec(ts, vec![])
}

#[inline]
fn name_and_arrow(
    ident: &mut String,
    mut iter: impl Iterator<Item = TokenTree>,
    so_far: &[String],
) {
    loop {
        // Expecting the `=` in `=>`:
        let Some(tree) = iter.next() else {
            panic!(
                "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but the function body ended",
                so_far.iter().fold(String::new(), |mut acc, s| {
                    let () = acc.push_str(s);
                    let () = acc.push('/');
                    acc
                })
            );
        };
        let TokenTree::Punct(punct) = tree else {
            panic!(
                "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found {tree:#?}",
                so_far.iter().fold(String::new(), |mut acc, s| {
                    let () = acc.push_str(s);
                    let () = acc.push('/');
                    acc
                })
            );
        };
        match punct.as_char() {
            '=' => { /* on track to see `=>`: fall through */ }
            '-' => {
                // Here's the only odd bit:
                // we want to allow hyphenation,
                // but Rust sees it as a minus sign.
                // So, even though we can't tell if there's space around it,
                // we want to treat a hyphen between names as one hyphenated name.
                let () = ident.push('-');
                let Some(tree) = iter.next() else {
                    panic!(
                        "Currently, this OSC library does not allow names ending in hyphens (specifically, we found `{ident}` after `/{}`). If you need to use an identifier ending in a hyphen, please open an issue or a PR.",
                        so_far.iter().fold(String::new(), |mut acc, s| {
                            let () = acc.push_str(s);
                            let () = acc.push('/');
                            acc
                        })
                    )
                };
                let TokenTree::Ident(id) = tree else {
                    panic!(
                        "In an OSC macro (after `/{}`), found a partial name (`{ident}`) with a hyphen, then {tree:#?} (but expected more name after the hyphen).",
                        so_far.iter().fold(String::new(), |mut acc, s| {
                            let () = acc.push_str(s);
                            let () = acc.push('/');
                            acc
                        })
                    )
                };
                let () = ident.push_str(&format!("{id}"));
                continue;
            }
            _ => panic!(
                "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found {punct:#?}",
                so_far.iter().fold(String::new(), |mut acc, s| {
                    let () = acc.push_str(s);
                    let () = acc.push('/');
                    acc
                })
            ),
        }
        if !matches!(punct.spacing(), Spacing::Joint) {
            panic!(
                "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found {punct:#?}",
                so_far.iter().fold(String::new(), |mut acc, s| {
                    let () = acc.push_str(s);
                    let () = acc.push('/');
                    acc
                })
            )
        }

        {
            // Expecting the `>` in `=>`:
            let Some(tree) = iter.next() else {
                panic!(
                    "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but the function body ended after the `=`",
                    so_far.iter().fold(String::new(), |mut acc, s| {
                        let () = acc.push_str(s);
                        let () = acc.push('/');
                        acc
                    })
                );
            };
            let TokenTree::Punct(punct) = tree else {
                panic!(
                    "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found `=` and then {tree:#?}",
                    so_far.iter().fold(String::new(), |mut acc, s| {
                        let () = acc.push_str(s);
                        let () = acc.push('/');
                        acc
                    })
                );
            };
            if !matches!(punct.as_char(), '>') {
                panic!(
                    "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found `=` and then {punct:#?}",
                    so_far.iter().fold(String::new(), |mut acc, s| {
                        let () = acc.push_str(s);
                        let () = acc.push('/');
                        acc
                    })
                )
            }
            if !matches!(punct.spacing(), Spacing::Alone) {
                panic!(
                    "Expected `=>` after the OSC container name `{ident}` (after `/{}`) but found `=` and then {punct:#?}",
                    so_far.iter().fold(String::new(), |mut acc, s| {
                        let () = acc.push_str(s);
                        let () = acc.push('/');
                        acc
                    })
                )
            }
        }

        return;
    }
}

#[inline]
fn call_or_more_cases(
    containers: &mut BTreeMap<Match, MaybeLeaf>,
    to_match: Match,
    mut iter: impl Iterator<Item = TokenTree>,
    so_far: &[String],
) {
    // Expecting either a function with argument types or another match:
    let Some(tree) = iter.next() else {
        panic!(
            "Expected either a function with argument types or another match after `=>` in an OSC macro but the function body ended"
        );
    };
    match tree {
        TokenTree::Group(group) => {
            if !matches!(group.delimiter(), Delimiter::Brace) {
                panic!(
                    "Expected either a function with argument types or another match after `=>` in an OSC macro but found {group:#?}"
                )
            }
            let so_far_clone = so_far
                .iter()
                .cloned()
                .chain(core::iter::once(to_match.print()))
                .collect();
            let overwritten = containers.insert(
                to_match.clone(),
                MaybeLeaf::Subtree(stratify_rec(group.stream(), so_far_clone)),
            );
            if let Some(overwritten) = overwritten {
                panic!(
                    "Duplicate mapping: {to_match:?} (after `/{}`) already mapped to {overwritten:#?}",
                    so_far.iter().fold(String::new(), |mut acc, s| {
                        let () = acc.push_str(s);
                        let () = acc.push('/');
                        acc
                    }),
                )
            }
        }
        TokenTree::Ident(fn_path_head) => {
            let mut fn_path_tail = vec![];
            let mut fn_args = vec![];
            'modpath: loop {
                let Some(tree) = iter.next() else {
                    panic!(
                        "Expected a function with argument types after `/{}{to_match:?}` but the function body ended after {fn_path_head:#?}",
                        so_far.iter().fold(String::new(), |mut acc, s| {
                            let () = acc.push_str(s);
                            let () = acc.push('/');
                            acc
                        }),
                    );
                };
                match tree {
                    TokenTree::Punct(punct) => {
                        if !matches!(punct.as_char(), ':') {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {punct:?} after {fn_path_head:#?}",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        }
                        if !matches!(punct.spacing(), Spacing::Joint) {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found a single colon (maybe should have been a path separator `::`?) after {fn_path_head:#?}",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        }
                        let Some(tree) = iter.next() else {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but the function body ended after {fn_path_head:#?} and a colon",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        };
                        let TokenTree::Punct(punct) = tree else {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {tree:?} after {fn_path_head:#?} and a colon",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        };
                        if !matches!(punct.as_char(), ':') {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {punct:?} after {fn_path_head:#?} and a colon",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        }
                        if !matches!(punct.spacing(), Spacing::Alone) {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {punct:?} after {fn_path_head:#?} and a colon",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        }
                        let Some(tree) = iter.next() else {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but the function body ended after {fn_path_head:#?} and a `::`",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        };
                        let TokenTree::Ident(id) = tree else {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {tree:?} after {fn_path_head:#?} and a `::`",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        };
                        let () = fn_path_tail.push(id);
                    }
                    TokenTree::Group(group) => {
                        if !matches!(group.delimiter(), Delimiter::Parenthesis) {
                            panic!(
                                "Expected a function with argument types after `/{}{to_match:?}` but found {group:?} after the path {fn_args:#?}",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            );
                        }
                        let mut iter = group.stream().into_iter();
                        'group_stream: loop {
                            let Some(tree) = iter.next() else {
                                break 'group_stream;
                            };
                            match tree {
                                TokenTree::Ident(ident) => {
                                    let () = fn_args.push(match format!("{ident}").as_str() {
                                        "int32" => Arg::Int32,
                                        "float32" => Arg::Float32,
                                        other => panic!("Unrecognized OSC argument type `{other}`: currently, the only supported types are `int32` and `float32`"),
                                    });
                                }
                                TokenTree::Punct(ref punct) => match punct.as_char() {
                                    ',' => {}
                                    '#' => {
                                        let Some(tree) = iter.next() else {
                                            panic!("OSC argument list ends in a hashtag")
                                        };
                                        let TokenTree::Ident(id) = tree else {
                                            panic!(
                                                "Hashtags in OSC argument lists should be followed by a variable name, but a hashtag was followed by {tree:#?}"
                                            )
                                        };
                                        let () = fn_args.push(Arg::PathInteger(id));
                                    }
                                    _ => panic!(
                                        "OSC callback function arguments should be type names like `int32`, but `{tree:#?}` was used as an argument"
                                    ),
                                },
                                _ => panic!(
                                    "OSC callback function arguments should be type names like `int32`, but `{tree:#?}` was used as an argument"
                                ),
                            }
                        }
                        let overwritten = containers.insert(
                            to_match.clone(),
                            MaybeLeaf::Leaf {
                                fn_path_head,
                                fn_path_tail,
                                fn_args,
                            },
                        );
                        if let Some(overwritten) = overwritten {
                            panic!(
                                "Duplicate mapping: {to_match:?} (after `/{}`) already mapped to {overwritten:#?}",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            )
                        }
                        break 'modpath;
                    }
                    _ => panic!(
                        "Expected a function with argument types after `/{}{to_match:?}` but found {tree:?} after {fn_path_head:#?} and {fn_args:#?}",
                        so_far.iter().fold(String::new(), |mut acc, s| {
                            let () = acc.push_str(s);
                            let () = acc.push('/');
                            acc
                        }),
                    ),
                }
            }
        }
        other => panic!(
            "Expected either a function with argument types or another match after `=>` in an OSC macro but found {other:#?}"
        ),
    }
}

#[inline]
fn stratify_rec(iter: impl IntoIterator<Item = TokenTree>, so_far: Vec<String>) -> Hierarchy {
    let mut iter = iter.into_iter();
    let mut containers = BTreeMap::<Match, MaybeLeaf>::new();
    loop {
        match iter.next() {
            Some(TokenTree::Ident(ident)) => {
                let mut ident = format!("{ident}");
                let () = name_and_arrow(&mut ident, &mut iter, &so_far);
                let () =
                    call_or_more_cases(&mut containers, Match::Name(ident), &mut iter, &so_far);
            }
            Some(TokenTree::Punct(punct)) => {
                match punct.as_char() {
                    ',' => { /* skip */ }
                    '#' => {
                        let Some(tree) = iter.next() else {
                            panic!(
                                "Expected an identifier representing an OSC variable container after `#` but the function body ended"
                            )
                        };
                        match tree {
                            TokenTree::Ident(ident) => {
                                let mut ident = format!("{ident}");
                                let () = name_and_arrow(&mut ident, &mut iter, &so_far);
                                let () = call_or_more_cases(
                                    &mut containers,
                                    Match::Integer(ident),
                                    &mut iter,
                                    &so_far,
                                );
                            }
                            other => panic!(
                                "Expected an identifier representing an OSC variable container after `#` but found {other:#?}"
                            ),
                        }
                    }
                    other => panic!(
                        "Expected an identifier representing an OSC container (between `/`s in a path) but found `{other:#?}` ({punct:#?})"
                    ),
                }
            }
            Some(other) => panic!(
                "Expected an identifier representing an OSC container (between `/`s in a path) but found {other:#?}"
            ),
            None => return Hierarchy { containers },
        }
    }
}

#[derive(Debug)]
pub(crate) struct Hierarchy {
    pub(crate) containers: BTreeMap<Match, MaybeLeaf>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Match {
    Name(String),
    Integer(String),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Pattern {
    ByteChar(u8),
    Integer(String),
}

#[derive(Debug)]
pub(crate) enum MaybeLeaf {
    Leaf {
        fn_path_head: Ident,
        fn_path_tail: Vec<Ident>,
        fn_args: Vec<Arg>,
    },
    Subtree(Hierarchy),
}

#[derive(Debug)]
pub(crate) enum Arg {
    Int32,
    Float32,
    PathInteger(Ident),
}

#[derive(Debug)]
pub(crate) struct Parser {
    cases: BTreeMap<Pattern, ParserState>,
    catch: String,
}

#[derive(Debug)]
pub(crate) enum ParserState {
    Complete(TokenStream),
    Incomplete(Parser),
    Assign {
        var_name: String,
        continuation: Parser,
    },
}

impl Hierarchy {
    #[inline]
    pub(crate) fn parser(self) -> Parser {
        // We're going to depth-first search the hierarchy,
        // adding each entire path character-by-character
        // before moving back up the tree and adding the next path,
        // merging paths wherever necessary, but only
        // *after* the first path has been fully added (again, DFS).

        let mut cases = BTreeMap::<Pattern, ParserState>::new();
        let () = parse_slash_etc(MaybeLeaf::Subtree(self), &mut cases, String::new());
        Parser {
            cases,
            catch: String::new(),
        }
    }
}

impl Match {
    #[inline]
    pub(crate) fn print(&self) -> String {
        match *self {
            Self::Name(ref name) => name.clone(),
            Self::Integer(ref name) => format!("#{name}"),
        }
    }
}

impl Parser {
    #[inline]
    pub(crate) fn into_tokens(self) -> impl IntoIterator<Item = TokenTree> {
        let v: Vec<_> = todo!("Parser::into_tokens: {self:#?}");
        v
    }
}

#[inline]
fn parse_slash_etc(
    maybe_leaf: MaybeLeaf,
    cases: &mut BTreeMap<Pattern, ParserState>,
    mut so_far: String,
) {
    match maybe_leaf {
        MaybeLeaf::Leaf {
            fn_path_head,
            fn_path_tail,
            fn_args,
        } => call_leaf(
            fn_path_head,
            fn_path_tail,
            fn_args,
            Pattern::ByteChar(b'/'),
            cases,
            so_far,
        ),
        MaybeLeaf::Subtree(Hierarchy { containers }) => {
            parse_subtree(Pattern::ByteChar(b'/'), cases, containers, so_far)
        }
    }
}

#[inline]
fn call_leaf(
    fn_path_head: Ident,
    fn_path_tail: Vec<Ident>,
    fn_args: Vec<Arg>,
    pattern: Pattern,
    cases: &mut BTreeMap<Pattern, ParserState>,
    mut so_far: String,
) {
    match cases.entry(pattern) {
        Entry::Vacant(vacant) => {
            let _: &mut _ = vacant.insert(ParserState::Complete(TokenStream::from_iter(
                core::iter::once(TokenTree::Ident(fn_path_head))
                    .chain(fn_path_tail.into_iter().flat_map(|id| {
                        [
                            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                            TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                            TokenTree::Ident(id),
                        ]
                    }))
                    .chain(core::iter::once(TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([TokenTree::Ident(Ident::new(
                            "TODO",
                            Span::call_site(),
                        ))]),
                    )))),
            )));
        }
        Entry::Occupied(occupied) => panic!(
            "Ambiguous OSC routing: `{so_far}` could either stop parsing and call or continue with {occupied:#?}"
        ),
    }
}

#[inline]
fn get_or_insert_match_case<'cases>(
    pattern: Pattern,
    cases: &'cases mut BTreeMap<Pattern, ParserState>,
    so_far: &mut String,
) -> &'cases mut BTreeMap<Pattern, ParserState> {
    let () = match pattern {
        Pattern::ByteChar(byte) => so_far.push(char::from(byte)),
        Pattern::Integer(ref name) => {
            let () = so_far.push('#');
            so_far.push_str(name)
        }
    };
    match cases.entry(pattern) {
        Entry::Vacant(vacant) => {
            let ParserState::Incomplete(Parser {
                ref mut cases,
                catch: _,
            }) = *vacant.insert(ParserState::Incomplete(Parser {
                cases: BTreeMap::new(),
                catch: so_far.clone(),
            }))
            else {
                unreachable!()
            };
            cases
        }
        Entry::Occupied(occupied) => {
            let extant = occupied.into_mut();
            match *extant {
                ParserState::Incomplete(Parser {
                    ref mut cases,
                    catch: _,
                }) => cases,
                ParserState::Complete(_) => panic!(
                    "Ambiguous OSC routing: after `{so_far}`, unclear whether to stop or to continue parsing this set of further routes: {extant:#?}"
                ),
                ParserState::Assign { .. } => panic!(
                    "Ambiguous OSC routing: after `{so_far}`, unclear whether to assign an integer or to continue parsing this set of further routes: {extant:#?}"
                ),
            }
        }
    }
}

#[inline]
fn parse_subtree(
    pattern: Pattern,
    cases: &mut BTreeMap<Pattern, ParserState>,
    containers: BTreeMap<Match, MaybeLeaf>,
    mut so_far: String,
) {
    let cases = get_or_insert_match_case(pattern, cases, &mut so_far);

    for (k, v) in containers {
        match k {
            Match::Name(name) => {
                let mut so_far = so_far.clone();
                let mut cases: *mut BTreeMap<Pattern, ParserState> = cases;
                for byte in name.into_bytes() {
                    cases = get_or_insert_match_case(
                        Pattern::ByteChar(byte),
                        unsafe { &mut *cases },
                        &mut so_far,
                    );
                }
                let () = parse_slash_etc(v, unsafe { &mut *cases }, so_far);
            }
            Match::Integer(name) => {
                let mut so_far = so_far.clone();
                let cases = get_or_insert_match_case(Pattern::Integer(name), cases, &mut so_far);
                let () = parse_slash_etc(v, cases, so_far);
            }
        }
    }
}

/*
#[inline]
fn char_by_char(mut name: String, cases: &mut BTreeMap<Pattern, ParserState>, so_far: String) {
    let Some(head) = tail.pop() else {
        panic!("OSC error: empty container name (after `{so_far}`)")
    };
}
*/
