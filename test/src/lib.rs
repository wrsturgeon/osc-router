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
            hyphenated-ident => an_async_fn(int32).await,
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

async fn an_async_fn(_: i32) {}

fn last_one(_: f32, _: i32) {}

fn j(_: u8) {}

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

fn f(_: i32, _: f32) {}

mod path {
    pub mod to {
        pub fn print_u8(_: u8) {}
    }
}
