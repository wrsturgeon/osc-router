//! OSC spec: <https://opensoundcontrol.stanford.edu/spec-1_0.html>

mod logic;

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

#[proc_macro]
pub fn osc(ts: TokenStream) -> TokenStream {
    let FnDecl {
        attributes,
        fn_name,
        generics,
        mut arg_stream,
        body,
    } = parse_function_declaration(ts);
    let hierarchy: logic::Hierarchy = logic::stratify(body);
    let parser = hierarchy.parser();

    let mut acc: TokenStream = TokenStream::from_iter(attributes);
    let () = acc.extend([
        TokenTree::Ident(Ident::new("async", Span::call_site())),
        TokenTree::Ident(Ident::new("fn", Span::call_site())),
        TokenTree::Ident(fn_name),
        TokenTree::Punct(Punct::new('<', Spacing::Alone)),
    ]);
    if !generics.is_empty() {
        let () = acc.extend(generics);
        let () = acc.extend(core::iter::once(TokenTree::Punct(Punct::new(
            ',',
            Spacing::Alone,
        ))));
    }
    let () = acc.extend([
        TokenTree::Ident(Ident::new("AsyncRestart", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Future", Span::call_site())),
        TokenTree::Punct(Punct::new('<', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Output", Span::call_site())),
        TokenTree::Punct(Punct::new('=', Spacing::Alone)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncRecvByte", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Future", Span::call_site())),
        TokenTree::Punct(Punct::new('<', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Output", Span::call_site())),
        TokenTree::Punct(Punct::new('=', Spacing::Alone)),
        TokenTree::Ident(Ident::new("u8", Span::call_site())),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncSendByte", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Future", Span::call_site())),
        TokenTree::Punct(Punct::new('<', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Output", Span::call_site())),
        TokenTree::Punct(Punct::new('=', Spacing::Alone)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncError", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Future", Span::call_site())),
        TokenTree::Punct(Punct::new('<', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Output", Span::call_site())),
        TokenTree::Punct(Punct::new('=', Spacing::Alone)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Restart", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("core", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("ops", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("FnMut", Span::call_site())),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Punct(Punct::new('-', Spacing::Joint)),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncRestart", Span::call_site())),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("RecvByte", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("core", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("ops", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("FnMut", Span::call_site())),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Punct(Punct::new('-', Spacing::Joint)),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncRecvByte", Span::call_site())),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("SendByte", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("core", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("ops", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("FnMut", Span::call_site())),
        TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter(core::iter::once(TokenTree::Ident(Ident::new(
                "u8",
                Span::call_site(),
            )))),
        )),
        TokenTree::Punct(Punct::new('-', Spacing::Joint)),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncSendByte", Span::call_site())),
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("Error", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("core", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("ops", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("FnMut", Span::call_site())),
        TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([
                TokenTree::Punct(Punct::new('&', Spacing::Alone)),
                TokenTree::Punct(Punct::new('\'', Spacing::Joint)),
                TokenTree::Ident(Ident::new("static", Span::call_site())),
                TokenTree::Ident(Ident::new("str", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("u8", Span::call_site())),
            ]),
        )),
        TokenTree::Punct(Punct::new('-', Spacing::Joint)),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Ident(Ident::new("AsyncError", Span::call_site())),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, {
            if !arg_stream.is_empty() {
                let () = arg_stream.extend(core::iter::once(TokenTree::Punct(Punct::new(
                    ',',
                    Spacing::Alone,
                ))));
            }
            let () = arg_stream.extend([
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("osc_router_traits", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("Driver", Span::call_site())),
                TokenTree::Group(Group::new(
                    Delimiter::Brace,
                    TokenStream::from_iter([
                        TokenTree::Ident(Ident::new("mut", Span::call_site())),
                        TokenTree::Ident(Ident::new("restart", Span::call_site())),
                        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                        TokenTree::Ident(Ident::new("mut", Span::call_site())),
                        TokenTree::Ident(Ident::new("recv_byte", Span::call_site())),
                        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                        TokenTree::Ident(Ident::new("mut", Span::call_site())),
                        TokenTree::Ident(Ident::new("send_byte", Span::call_site())),
                        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                        TokenTree::Ident(Ident::new("mut", Span::call_site())),
                        TokenTree::Ident(Ident::new("error", Span::call_site())),
                    ]),
                )),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("osc_router_traits", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                TokenTree::Ident(Ident::new("Driver", Span::call_site())),
                TokenTree::Punct(Punct::new('<', Spacing::Alone)),
                TokenTree::Ident(Ident::new("AsyncRestart", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("AsyncRecvByte", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("AsyncSendByte", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("AsyncError", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("Restart", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("RecvByte", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("SendByte", Span::call_site())),
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Ident(Ident::new("Error", Span::call_site())),
                TokenTree::Punct(Punct::new('>', Spacing::Alone)),
            ]);
            arg_stream
        })),
        TokenTree::Punct(Punct::new('-', Spacing::Joint)),
        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
        TokenTree::Punct(Punct::new('!', Spacing::Alone)),
        TokenTree::Group(Group::new(
            Delimiter::Brace,
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new("loop", Span::call_site())),
                TokenTree::Group(Group::new(
                    Delimiter::Brace,
                    TokenStream::from_iter(
                        [
                            TokenTree::Ident(Ident::new("let", Span::call_site())),
                            TokenTree::Group(Group::new(
                                Delimiter::Parenthesis,
                                TokenStream::new(),
                            )),
                            TokenTree::Punct(Punct::new('=', Spacing::Alone)),
                            TokenTree::Ident(Ident::new("restart", Span::call_site())),
                            TokenTree::Group(Group::new(
                                Delimiter::Parenthesis,
                                TokenStream::new(),
                            )),
                            TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                            TokenTree::Ident(Ident::new("await", Span::call_site())),
                            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
                        ]
                        .into_iter()
                        .chain(parser.into_tokens()),
                    ),
                )),
            ]),
        )),
    ]);
    acc
}

#[derive(Debug)]
struct FnDecl {
    pub attributes: Vec<TokenTree>,
    pub fn_name: Ident,
    pub generics: Vec<TokenTree>,
    pub arg_stream: TokenStream,
    pub body: TokenStream,
}

#[inline]
fn parse_function_declaration(iter: impl IntoIterator<Item = TokenTree>) -> FnDecl {
    let mut iter = iter.into_iter();

    let mut attributes = vec![];

    // Iterate through all the attribute macros (`#[...]`):
    'attributes: loop {
        match iter.next() {
            None => panic!(
                "Expected a function declaration in an OSC router macro, but the macro body ended"
            ),
            Some(TokenTree::Punct(punct)) => match punct.as_char() {
                '#' => match iter.next() {
                    None => panic!(
                        "Expected a function declaration in an OSC router macro, but the macro body ended after a hashtag"
                    ),
                    Some(TokenTree::Group(group)) => match group.delimiter() {
                        Delimiter::Bracket => {
                            let () = attributes.push(TokenTree::Punct(punct));
                            let () = attributes.push(TokenTree::Group(group));
                        }
                        other => panic!(
                            "Expected an attribute body (`[...]`) after a hashtag in an OSC router macro but found a body enclosed in {other:#?} tokens"
                        ),
                    },
                    Some(other) => panic!(
                        "Expected an attribute body (`[...]`) after a hashtag in an OSC router macro but found {other:#?}"
                    ),
                },
                c => panic!("Unrecognized punctuation in an OSC router macro: {c:#?}"),
            },
            Some(TokenTree::Ident(ident)) => match format!("{ident}").as_str() {
                "pub" => {
                    let () = attributes.push(TokenTree::Ident(ident));
                }
                "async" => break 'attributes,
                _ => panic!(
                    "Expected `pub` or `async` to begin the function declaration in an OSC router macro but found {ident:#?}"
                ),
            },
            Some(other) => {
                panic!(
                    "Expected a function declaration in an OSC router macro but found {other:#?}"
                )
            }
        }
    }

    {
        // Expecting `fn`:
        let Some(tree) = iter.next() else {
            panic!("Expected `fn` after `async` in an OSC router macro but the macro body ended")
        };
        let TokenTree::Ident(ident) = tree else {
            panic!("Expected `fn` after `async` in an OSC router macro but found {tree:#?}")
        };
        if !matches!(format!("{ident}").as_str(), "fn") {
            panic!("Expected `fn` after `async` in an OSC router macro but found {ident:#?}")
        }
    }

    let fn_name = {
        // Expecting the function name:
        let Some(tree) = iter.next() else {
            panic!(
                "Expected a function name after `fn` in an OSC router macro but the macro body ended"
            )
        };
        let TokenTree::Ident(ident) = tree else {
            panic!("Expected a function name after `fn` in an OSC router macro but found {tree:#?}")
        };
        ident
    };

    let mut generics = vec![];
    let arg_stream = {
        // Expecting parenthesized arguments (but no arguments, so really just `()`):
        let Some(mut tree) = iter.next() else {
            panic!(
                "Expected arguments after the function name in an OSC router macro but the macro body ended"
            )
        };
        if let TokenTree::Punct(ref punct) = tree {
            if !matches!(punct.as_char(), '<') {
                panic!(
                    "Expected arguments after the function name in an OSC router macro but found {tree:#?}"
                )
            }
            let mut angle_bracket_inception: usize = 1;
            'generics: loop {
                tree = iter.next().expect(
                        "Function generics in an OSC router macro missing a closing `>` (maybe off-by-one error?)"
                );
                if let TokenTree::Punct(ref punct) = tree {
                    match punct.as_char() {
                        '<' => angle_bracket_inception += 1,
                        '>' => {
                            angle_bracket_inception -= 1;
                            if angle_bracket_inception == 0 {
                                tree = iter.next().expect(
                                    "Function generics in an OSC router macro missing a closing `>` (maybe off-by-one error?)"
                                );
                                break 'generics;
                            }
                        }
                        _ => {}
                    }
                }
                let () = generics.push(tree);
            }
        }
        let TokenTree::Group(group) = tree else {
            panic!(
                "Expected arguments after the function name in an OSC router macro but found {tree:#?}"
            )
        };
        group.stream()
    };

    {
        let Some(tree) = iter.next() else {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but the macro body ended"
            )
        };
        let TokenTree::Punct(punct) = tree else {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found {tree:#?}"
            )
        };
        if !matches!(punct.as_char(), '-') {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found {punct:#?}"
            )
        }
        if !matches!(punct.spacing(), Spacing::Joint) {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found {punct:#?}"
            )
        }
    }

    {
        let Some(tree) = iter.next() else {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but the macro body ended"
            )
        };
        let TokenTree::Punct(punct) = tree else {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found `-` and then {tree:#?}"
            )
        };
        if !matches!(punct.as_char(), '>') {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found `-` and then {punct:#?}"
            )
        }
        if !matches!(punct.spacing(), Spacing::Alone) {
            panic!(
                "Expected `->` after function arguments in an OSC router macro but found `-` and then {punct:#?}"
            )
        }
    }

    {
        let Some(tree) = iter.next() else {
            panic!(
                "Expected `!` as the return type of the function in an OSC router macro but the macro body ended"
            )
        };
        let TokenTree::Punct(punct) = tree else {
            panic!(
                "Expected `!` as the return type of the function in an OSC router macro but found {tree:#?}"
            )
        };
        if !matches!(punct.as_char(), '!') {
            panic!(
                "Expected `!` as the return type of the function in an OSC router macro but found {punct:#?}"
            )
        }
        if !matches!(punct.spacing(), Spacing::Alone) {
            panic!(
                "Expected `!` as the return type of the function in an OSC router macro but found {punct:#?}"
            )
        }
    }

    let body = {
        let Some(tree) = iter.next() else {
            panic!(
                "Expected a function body after `-> !` in an OSC router macro but the macro body ended"
            )
        };
        let TokenTree::Group(group) = tree else {
            panic!(
                "Expected a function body after `-> !` in an OSC router macro but found {tree:#?}"
            )
        };
        if !matches!(group.delimiter(), Delimiter::Brace) {
            panic!(
                "Expected a function body after `-> !` in an OSC router macro but found {group:#?}"
            )
        }
        group.stream()
    };

    if let Some(extra) = iter.next() {
        panic!("Expected the OSC router macro to end after a single function but found {extra:#?}");
    }

    FnDecl {
        attributes,
        fn_name,
        generics,
        arg_stream,
        body,
    }
}
