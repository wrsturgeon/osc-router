#![no_std]

osc_router::osc! {
    #[inline]
    pub async fn osc_server() -> ! {
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
    }
}

fn comma() {}

fn conflict() {}

fn no_comma(_: i32) {}

fn last_one(_: f32, _: i32) {}
