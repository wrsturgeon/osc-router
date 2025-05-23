#![no_std]

pub trait Send {
    fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(
        &self,
        f: F,
    ) -> impl Future<Output = ()>;
}

impl Send for () {
    #[inline]
    async fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(&self, mut f: F) {
        let () = f(b'/').await;
    }
}

impl<Some: Send> Send for Option<Some> {
    #[inline]
    async fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(&self, mut f: F) {
        let () = f(b'/').await;
        let () = match *self {
            None => {
                let () = f(b'n').await;
                let () = f(b'o').await;
                let () = f(b'n').await;
                let () = f(b'e').await;
            }
            Some(ref some) => {
                let () = f(b's').await;
                let () = f(b'o').await;
                let () = f(b'm').await;
                let () = f(b'e').await;
                let () = some.send_osc(f).await;
            }
        };
    }
}

impl<Ok: Send, Err: Send> Send for Result<Ok, Err> {
    #[inline]
    async fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(&self, mut f: F) {
        let () = f(b'/').await;
        let () = match *self {
            Ok(ref ok) => {
                let () = f(b'o').await;
                let () = f(b'k').await;
                let () = ok.send_osc(f).await;
            }
            Err(ref err) => {
                let () = f(b'e').await;
                let () = f(b'r').await;
                let () = f(b'r').await;
                let () = err.send_osc(f).await;
            }
        };
    }
}

pub struct Driver<
    AsyncRestart: Future<Output = ()>,
    AsyncRecvByte: Future<Output = u8>,
    AsyncSendByte: Future<Output = ()>,
    AsyncError: Future<Output = ()>,
    Restart: FnMut() -> AsyncRestart,
    RecvByte: FnMut() -> AsyncRecvByte,
    SendByte: FnMut(u8) -> AsyncSendByte,
    Error: FnMut(&'static str, u8) -> AsyncError,
> {
    pub restart: Restart,
    pub recv_byte: RecvByte,
    pub send_byte: SendByte,
    pub error: Error,
}
