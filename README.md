# osc-router

Zero-copy OSC router for `no_std` Rust.

### Examples

Turns this:

```rust
osc_router::osc! {
    #[inline]
    pub async fn your_fn_name() -> ! {
        pre => {
            a => f(),
            b => g(int32),
            c => h(float32, int32),
        }
        prefix => {
            hello => goodbye(),
        }
    }
}
```

into this:

```rust
#[inline]
pub async fn your_fn_name<
    AsyncByte: Future<Output = u8>,
    NextByte: FnMut() -> AsyncByte,
    Error: FnMut(&str, u8),
>(mut next_byte: NextByte, mut error: Error) -> ! {
    loop {
        match next_byte().await {
            b'/' => {
                match next_byte().await {
                    b'p' => {
                        match next_byte().await {
                            b'r' => {
                                match next_byte().await {
                                    b'e' => {
                                        match next_byte().await {
                                            b'/' => {
                                                match next_byte().await {
                                                    b'a' => {
                                                        match next_byte().await {
                                                            b'/' => f(),
                                                            unexpected => error("/pre/a", unexpected),
                                                        }
                                                    }
                                                    b'b' => {
                                                        match next_byte().await {
                                                            b'/' => {
                                                                g(
                                                                    i32::from_be_bytes([
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                    ]),
                                                                )
                                                            }
                                                            unexpected => error("/pre/b", unexpected),
                                                        }
                                                    }
                                                    b'c' => {
                                                        match next_byte().await {
                                                            b'/' => {
                                                                h(
                                                                    f32::from_be_bytes([
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                    ]),
                                                                    i32::from_be_bytes([
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                        next_byte().await,
                                                                    ]),
                                                                )
                                                            }
                                                            unexpected => error("/pre/c", unexpected),
                                                        }
                                                    }
                                                    unexpected => error("/pre/", unexpected),
                                                }
                                            }
                                            b'f' => {
                                                match next_byte().await {
                                                    b'i' => {
                                                        match next_byte().await {
                                                            b'x' => {
                                                                match next_byte().await {
                                                                    b'/' => {
                                                                        match next_byte().await {
                                                                            b'h' => {
                                                                                match next_byte().await {
                                                                                    b'e' => {
                                                                                        match next_byte().await {
                                                                                            b'l' => {
                                                                                                match next_byte().await {
                                                                                                    b'l' => {
                                                                                                        match next_byte().await {
                                                                                                            b'o' => {
                                                                                                                match next_byte().await {
                                                                                                                    b'/' => goodbye(),
                                                                                                                    unexpected => error("/prefix/hello", unexpected),
                                                                                                                }
                                                                                                            }
                                                                                                            unexpected => error("/prefix/hell", unexpected),
                                                                                                        }
                                                                                                    }
                                                                                                    unexpected => error("/prefix/hel", unexpected),
                                                                                                }
                                                                                            }
                                                                                            unexpected => error("/prefix/he", unexpected),
                                                                                        }
                                                                                    }
                                                                                    unexpected => error("/prefix/h", unexpected),
                                                                                }
                                                                            }
                                                                            unexpected => error("/prefix/", unexpected),
                                                                        }
                                                                    }
                                                                    unexpected => error("/prefix", unexpected),
                                                                }
                                                            }
                                                            unexpected => error("/prefi", unexpected),
                                                        }
                                                    }
                                                    unexpected => error("/pref", unexpected),
                                                }
                                            }
                                            unexpected => error("/pre", unexpected),
                                        }
                                    }
                                    unexpected => error("/pr", unexpected),
                                }
                            }
                            unexpected => error("/p", unexpected),
                        }
                    }
                    unexpected => error("/", unexpected),
                }
            }
            unexpected => error("", unexpected),
        }
    }
}
```
