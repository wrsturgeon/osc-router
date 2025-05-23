pub trait Osc {
    // async fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(&self, f: F);
    fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(
        &self,
        f: F,
    ) -> impl Future<Output = ()>;
}

impl Osc for () {
    #[inline]
    async fn send_osc<AsyncUnit: Future<Output = ()>, F: FnMut(u8) -> AsyncUnit>(&self, mut f: F) {
        let () = f(b'/').await;
    }
}
