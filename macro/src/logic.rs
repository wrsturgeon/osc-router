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
fn stratify_rec(iter: impl IntoIterator<Item = TokenTree>, so_far: Vec<String>) -> Hierarchy {
    let mut iter = iter.into_iter();
    let mut containers = BTreeMap::<String, MaybeLeaf>::new();
    loop {
        match iter.next() {
            Some(TokenTree::Ident(ident)) => {
                let mut ident = format!("{ident}");

                'maybe_hyphenated: loop {
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
                            continue 'maybe_hyphenated;
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
                    break 'maybe_hyphenated;
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
                            .chain(core::iter::once(ident.clone()))
                            .collect();
                        let overwritten = containers.insert(
                            ident.clone(),
                            MaybeLeaf::Subtree(stratify_rec(group.stream(), so_far_clone)),
                        );
                        if let Some(overwritten) = overwritten {
                            panic!(
                                "Duplicate mapping: {ident} (after `/{}`) already mapped to {overwritten:#?}",
                                so_far.iter().fold(String::new(), |mut acc, s| {
                                    let () = acc.push_str(s);
                                    let () = acc.push('/');
                                    acc
                                }),
                            )
                        }
                    }
                    TokenTree::Ident(fn_ident) => {
                        let mut tokens = vec![TokenTree::Ident(fn_ident)];
                        'modpath: loop {
                            let Some(tree) = iter.next() else {
                                panic!(
                                    "Expected a function with argument types after `/{}{ident}` but the function body ended after {}",
                                    so_far.iter().fold(String::new(), |mut acc, s| {
                                        let () = acc.push_str(s);
                                        let () = acc.push('/');
                                        acc
                                    }),
                                    TokenStream::from_iter(tokens.into_iter())
                                );
                            };
                            match tree {
                                TokenTree::Punct(punct) => {
                                    if !matches!(punct.as_char(), ':') {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {punct:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    }
                                    if !matches!(punct.spacing(), Spacing::Joint) {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found a single colon (maybe should have been a path separator `::`?) after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    }
                                    let () = tokens.push(TokenTree::Punct(punct));
                                    let Some(tree) = iter.next() else {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but the function body ended after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    };
                                    let TokenTree::Punct(punct) = tree else {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {tree:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    };
                                    if !matches!(punct.as_char(), ':') {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {punct:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    }
                                    if !matches!(punct.spacing(), Spacing::Alone) {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {punct:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    }
                                    let () = tokens.push(TokenTree::Punct(punct));
                                    let Some(tree) = iter.next() else {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but the function body ended after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    };
                                    let TokenTree::Ident(id) = tree else {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {tree:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    };
                                    let () = tokens.push(TokenTree::Ident(id));
                                }
                                TokenTree::Group(group) => {
                                    if !matches!(group.delimiter(), Delimiter::Parenthesis) {
                                        panic!(
                                            "Expected a function with argument types after `/{}{ident}` but found {group:?} after {}",
                                            so_far.iter().fold(String::new(), |mut acc, s| {
                                                let () = acc.push_str(s);
                                                let () = acc.push('/');
                                                acc
                                            }),
                                            TokenStream::from_iter(tokens.into_iter())
                                        );
                                    }
                                    let () = tokens.push(TokenTree::Group(Group::new(
                                        Delimiter::Parenthesis,
                                        TokenStream::from_iter(group.stream().into_iter().flat_map(
                                            |tree| match tree {
                                                TokenTree::Ident(ident) => {
                                                    let rust_type = match format!("{ident}").as_str() {
                                                        "int32" => "i32",
                                                        "float32" => "f32",
                                                        _ => panic!("Unrecognized OSC argument type `{ident}`: currently, the only supported types are `i32` and `f32`"),
                                                    };
                                                    vec![
                                                        TokenTree::Ident(Ident::new(rust_type, Span::call_site())),
                                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                                        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                                                        TokenTree::Ident(Ident::new("from_be_bytes", Span::call_site())),
                                                        TokenTree::Group(Group::new(Delimiter::Parenthesis,
                                                            TokenStream::from_iter(core::iter::once(TokenTree::Group(Group::new(Delimiter::Bracket,
                                                                TokenStream::from_iter([
                                                                    TokenTree::Ident(Ident::new("next_byte", Span::call_site())),
                                                                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                                                                    TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("await", Span::call_site())),
                                                                    TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("next_byte", Span::call_site())),
                                                                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                                                                    TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("await", Span::call_site())),
                                                                    TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("next_byte", Span::call_site())),
                                                                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                                                                    TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("await", Span::call_site())),
                                                                    TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("next_byte", Span::call_site())),
                                                                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                                                                    TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                                                                    TokenTree::Ident(Ident::new("await", Span::call_site())),
                                                                ])
                                                            ))))
                                                        )),
                                                    ]
                                                },
                                                TokenTree::Punct(ref punct) => {
                                                    match punct.as_char() {
                                                        ',' => vec![tree],
                                                        _ => panic!("OSC callback function arguments should be type names like `i32`, but `{tree:#?}` was used as an argument"),
                                                    }
                                                }
                                                _ => panic!("OSC callback function arguments should be type names like `i32`, but `{tree:#?}` was used as an argument"),
                                            }
                                        )),
                                    )));
                                    let overwritten =
                                        containers.insert(ident.clone(), MaybeLeaf::Leaf(tokens));
                                    if let Some(overwritten) = overwritten {
                                        panic!(
                                            "Duplicate mapping: {ident} (after `/{}`) already mapped to {overwritten:#?}",
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
                                    "Expected a function with argument types after `/{}{ident}` but found {tree:?} after {}",
                                    so_far.iter().fold(String::new(), |mut acc, s| {
                                        let () = acc.push_str(s);
                                        let () = acc.push('/');
                                        acc
                                    }),
                                    TokenStream::from_iter(tokens.into_iter())
                                ),
                            }
                        }
                    }
                    other => panic!(
                        "Expected either a function with argument types or another match after `=>` in an OSC macro but found {other:#?}"
                    ),
                }
            }
            Some(TokenTree::Punct(punct)) => {
                if !matches!(punct.as_char(), ',') {
                    panic!(
                        "Expected an identifier representing an OSC container (between `/`s in a path) but found {punct:#?}"
                    )
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
    pub(crate) containers: BTreeMap<String, MaybeLeaf>,
}

#[derive(Debug)]
pub(crate) enum MaybeLeaf {
    Leaf(Vec<TokenTree>),
    Subtree(Hierarchy),
}

/*
#[derive(Debug)]
pub(crate) enum Untangling {
    Stop(Vec<TokenTree>),
    Go(BTreeMap<Vec<u8>, MaybeLeaf>),
}

#[derive(Debug)]
pub(crate) enum Parser {
    Call(Vec<TokenTree>),
    Case(BTreeMap<u8, Parser>),
}
*/

#[derive(Debug)]
pub(crate) struct Parser {
    cases: BTreeMap<u8, ParserState>,
    catch: String,
}

#[derive(Debug)]
pub(crate) enum ParserState {
    Complete(Vec<TokenTree>),
    Incomplete(Parser),
}

impl Hierarchy {
    #[inline]
    pub(crate) fn parser(self) -> Parser {
        // We're going to depth-first search the hierarchy,
        // adding each entire path character-by-character
        // before moving back up the tree and adding the next path,
        // merging paths wherever necessary, but only
        // *after* the first path has been fully added (again, DFS).

        let mut cases = BTreeMap::<u8, ParserState>::new();
        let () = incrementalize(self, &mut cases, String::new());
        Parser {
            cases,
            catch: String::new(),
        }
    }
}

#[inline]
fn incrementalize(
    Hierarchy { containers }: Hierarchy,
    cases: &mut BTreeMap<u8, ParserState>,
    mut so_far: String,
) {
    let mut after_slash = BTreeMap::new();
    let () = so_far.push('/');
    for (k, v) in containers {
        let mut k = k.into_bytes();
        let () = k.reverse();
        let Some(c) = k.pop() else {
            panic!("Empty OSC container name after `{so_far}`");
        };
        let mut so_far = so_far.clone();
        let () = so_far.push(char::from(c));
        let entry = after_slash.entry(c);
        let () = merge(entry, k, v, so_far);
    }
    let overwritten = cases.insert(
        b'/',
        ParserState::Incomplete(Parser {
            cases: after_slash,
            catch: so_far.clone(),
        }),
    );
    if let Some(overwritten) = overwritten {
        panic!("Duplicate mapping of `/` after `{so_far}` (overwrote {overwritten:#?})")
    }
}

#[inline]
fn merge(
    entry: Entry<u8, ParserState>,
    mut tail_reversed: Vec<u8>,
    maybe_leaf: MaybeLeaf,
    mut so_far: String,
) {
    let Some(c) = tail_reversed.pop() else {
        return match maybe_leaf {
            MaybeLeaf::Leaf(tokens) => match entry {
                Entry::Vacant(vacant) => {
                    let _: &mut _ = vacant.insert(ParserState::Incomplete(Parser {
                        cases: core::iter::once((b'/', ParserState::Complete(tokens))).collect(),
                        catch: so_far,
                    }));
                }
                Entry::Occupied(occupied) => panic!(
                    "Ambiguous OSC routes: `{so_far}` could either end and call `{tokens:#?}` or follow this existing path: `{occupied:#?}`"
                ),
            },
            MaybeLeaf::Subtree(hierarchy) => match entry {
                Entry::Vacant(vacant) => {
                    let mut cases = BTreeMap::new();
                    let () = incrementalize(hierarchy, &mut cases, so_far.clone());
                    let _: &mut _ = vacant.insert(ParserState::Incomplete(Parser {
                        cases,
                        catch: so_far,
                    }));
                }
                Entry::Occupied(occupied) => panic!(
                    "Ambiguous OSC routes: `{so_far}` could either continue by parsing a `/` or follow this existing path: `{occupied:#?}`"
                ),
            },
        };
    };

    match entry {
        Entry::Vacant(vacant) => {
            let mut cases = BTreeMap::new();
            let entry = cases.entry(c);
            {
                let mut so_far_extn = so_far.clone();
                let () = so_far_extn.push(char::from(c));
                let () = merge(entry, tail_reversed, maybe_leaf, so_far_extn);
            }
            let _: &mut _ = vacant.insert(ParserState::Incomplete(Parser {
                cases,
                catch: so_far,
            }));
        }
        Entry::Occupied(occupied) => match *occupied.into_mut() {
            ParserState::Complete(ref tokens) => panic!(
                "Ambiguous OSC routes: `{so_far}` could either end and call `{tokens:#?}` or continue and parse `{:#?}`",
                {
                    let () = tail_reversed.reverse();
                    String::from_utf8(tail_reversed)
                }
            ),
            ParserState::Incomplete(Parser {
                ref mut cases,
                catch: _,
            }) => {
                let entry = cases.entry(c);
                let () = so_far.push(char::from(c));
                let () = merge(entry, tail_reversed, maybe_leaf, so_far);
            }
        },
    }
}

impl Parser {
    #[inline]
    pub fn into_tokens(self) -> [TokenTree; 6] {
        let Self { cases, catch } = self;
        let mut match_body = TokenStream::new();
        for (k, v) in cases {
            let () = match_body.extend([
                TokenTree::Literal(Literal::byte_character(k)),
                TokenTree::Punct(Punct::new('=', Spacing::Joint)),
                TokenTree::Punct(Punct::new('>', Spacing::Alone)),
            ]);
            let () = match v {
                ParserState::Complete(tokens) => match_body.extend(tokens),
                ParserState::Incomplete(inception) => match_body.extend(inception.into_tokens()),
            };
            let () = match_body.extend(core::iter::once(TokenTree::Punct(Punct::new(
                ',',
                Spacing::Alone,
            ))));
        }
        let () = match_body.extend([
            TokenTree::Ident(Ident::new("unexpected", Span::call_site())),
            TokenTree::Punct(Punct::new('=', Spacing::Joint)),
            TokenTree::Punct(Punct::new('>', Spacing::Alone)),
            TokenTree::Ident(Ident::new("error", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([
                    TokenTree::Literal(Literal::string(&catch)),
                    TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("unexpected", Span::call_site())),
                ]),
            )),
            TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        ]);
        [
            TokenTree::Ident(Ident::new("match", Span::call_site())),
            TokenTree::Ident(Ident::new("next_byte", Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
            TokenTree::Punct(Punct::new('.', Spacing::Alone)),
            TokenTree::Ident(Ident::new("await", Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Brace, match_body)),
        ]
    }
}
