#![no_std]

osc_router::osc! {
    #[inline]
    pub async fn osc_server<G: Generic>(_: G) -> ! {
        comma => {
            hyphenated-ident => crate::comma(),
        },
        conflict => {
            hyphenated-ident => conflict(),
        },
        no_comma => {
            hyphenated-ident => no_comma(int32),
        }
        last_one => {
            hyphenated-ident => last_one(float32, int32,),
        }
        pre => {}
        prefix => {}
        #i => j(#i)
    }
}

trait Generic {}

fn comma() {}

fn conflict() {}

fn no_comma(_: i32) {}

fn last_one(_: f32, _: i32) {}

fn j(_: u8) {}

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

fn f() {}

fn g(_: i32) {}

fn h(_: f32, _: i32) {}

fn goodbye() {}
