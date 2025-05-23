# osc-router

Zero-copy OSC router for `no_std` Rust.

### Examples

Turns this:

```rust
osc_router::osc! {
    #[inline]
    pub async fn your_fn_name() -> ! {
        pre => {
            a => f(int32, float32),
        }
        prefix => {
            #i => {
                print => path::to::print_u8(#i),
            }
        }
    }
}
```

into this:

```rust
#[inline]
pub async fn your_fn_name<
    AsyncRestart: Future<Output = ()>,
    AsyncRecvByte: Future<Output = u8>,
    AsyncSendByte: Future<Output = ()>,
    AsyncError: Future<Output = ()>,
    Restart: FnMut() -> AsyncRestart,
    RecvByte: FnMut() -> AsyncRecvByte,
    SendByte: FnMut(u8) -> AsyncSendByte,
    Error: FnMut(&str, u8) -> AsyncError,
>(
    ::osc_router_traits::Driver {
        mut restart,
        mut recv_byte,
        mut send_byte,
        mut error,
    }: ::osc_router_traits::Driver<
        AsyncRestart,
        AsyncRecvByte,
        AsyncSendByte,
        AsyncError,
        Restart,
        RecvByte,
        SendByte,
        Error,
    >,
) -> ! {
    loop {
        let () = restart().await;
        match recv_byte().await {
            b'/' => {
                match recv_byte().await {
                    b'p' => {
                        match recv_byte().await {
                            b'r' => {
                                match recv_byte().await {
                                    b'e' => {
                                        match recv_byte().await {
                                            b'/' => {
                                                match recv_byte().await {
                                                    b'a' => {
                                                        match recv_byte().await {
                                                            b'/' => {
                                                                let () = ::osc_router_traits::Send::send_osc(
                                                                        &f(
                                                                            i32::from_be_bytes([
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                            ]),
                                                                            f32::from_be_bytes([
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                                recv_byte().await,
                                                                            ]),
                                                                        ),
                                                                        &mut send_byte,
                                                                    )
                                                                    .await;
                                                            }
                                                            unexpected => {
                                                                let () = error("/pre/a", unexpected).await;
                                                            }
                                                        }
                                                    }
                                                    unexpected => {
                                                        let () = error("/pre/", unexpected).await;
                                                    }
                                                }
                                            }
                                            b'f' => {
                                                match recv_byte().await {
                                                    b'i' => {
                                                        match recv_byte().await {
                                                            b'x' => {
                                                                match recv_byte().await {
                                                                    b'/' => {
                                                                        match recv_byte().await {
                                                                            digit @ (b'0'..=b'9') => {
                                                                                let mut i: u8 = digit - b'0';
                                                                                'i: loop {
                                                                                    match recv_byte().await {
                                                                                        digit @ (b'0'..=b'9') => {
                                                                                            i = 10u8 * i + (digit - b'0');
                                                                                            continue 'i;
                                                                                        }
                                                                                        b'/' => {
                                                                                            match recv_byte().await {
                                                                                                b'p' => {
                                                                                                    match recv_byte().await {
                                                                                                        b'r' => {
                                                                                                            match recv_byte().await {
                                                                                                                b'i' => {
                                                                                                                    match recv_byte().await {
                                                                                                                        b'n' => {
                                                                                                                            match recv_byte().await {
                                                                                                                                b't' => {
                                                                                                                                    match recv_byte().await {
                                                                                                                                        b'/' => {
                                                                                                                                            let () = ::osc_router_traits::Send::send_osc(
                                                                                                                                                    &path::to::print_u8(i),
                                                                                                                                                    &mut send_byte,
                                                                                                                                                )
                                                                                                                                                .await;
                                                                                                                                        }
                                                                                                                                        unexpected => {
                                                                                                                                            let () = error("/prefix/#i/print", unexpected).await;
                                                                                                                                        }
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                unexpected => {
                                                                                                                                    let () = error("/prefix/#i/prin", unexpected).await;
                                                                                                                                }
                                                                                                                            }
                                                                                                                        }
                                                                                                                        unexpected => {
                                                                                                                            let () = error("/prefix/#i/pri", unexpected).await;
                                                                                                                        }
                                                                                                                    }
                                                                                                                }
                                                                                                                unexpected => {
                                                                                                                    let () = error("/prefix/#i/pr", unexpected).await;
                                                                                                                }
                                                                                                            }
                                                                                                        }
                                                                                                        unexpected => {
                                                                                                            let () = error("/prefix/#i/p", unexpected).await;
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                                unexpected => {
                                                                                                    let () = error("/prefix/#i/", unexpected).await;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                        unexpected => {
                                                                                            let () = error("/prefix/#i", unexpected).await;
                                                                                        }
                                                                                    }
                                                                                    break 'i;
                                                                                }
                                                                            }
                                                                            unexpected => {
                                                                                let () = error("/prefix/", unexpected).await;
                                                                            }
                                                                        }
                                                                    }
                                                                    unexpected => {
                                                                        let () = error("/prefix", unexpected).await;
                                                                    }
                                                                }
                                                            }
                                                            unexpected => {
                                                                let () = error("/prefi", unexpected).await;
                                                            }
                                                        }
                                                    }
                                                    unexpected => {
                                                        let () = error("/pref", unexpected).await;
                                                    }
                                                }
                                            }
                                            unexpected => {
                                                let () = error("/pre", unexpected).await;
                                            }
                                        }
                                    }
                                    unexpected => {
                                        let () = error("/pr", unexpected).await;
                                    }
                                }
                            }
                            unexpected => {
                                let () = error("/p", unexpected).await;
                            }
                        }
                    }
                    unexpected => {
                        let () = error("/", unexpected).await;
                    }
                }
            }
            unexpected => {
                let () = error("", unexpected).await;
            }
        }
    }
}
```
