use repetitive::repetitive;

repetitive! {
    trait Even {}
    trait Odd {}

    @for i in 0..4 {
        @let NumberI = @['_Number @i];

        struct @NumberI;

        @if i % 2 == 0 {
            impl Even for @NumberI {}
        } else {
            impl Odd for @NumberI {}
        }
    }
}

const fn _verify_even<T: Even>() {}
const fn _verify_odd<T: Odd>() {}

const _: () = _verify_even::<_Number0>();
const _: () = _verify_odd::<_Number1>();
const _: () = _verify_even::<_Number2>();
const _: () = _verify_odd::<_Number3>();
