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

pub struct Driver<
    AsyncRestart: Future<Output = ()>,
    AsyncRecvByte: Future<Output = u8>,
    AsyncSendByte: Future<Output = ()>,
    AsyncError: Future<Output = ()>,
    Restart: FnMut() -> AsyncRestart,
    RecvByte: FnMut() -> AsyncRecvByte,
    SendByte: FnMut(u8) -> AsyncSendByte,
    Error: FnMut(&str, u8) -> AsyncError,
> {
    pub restart: Restart,
    pub recv_byte: RecvByte,
    pub send_byte: SendByte,
    pub error: Error,
}
