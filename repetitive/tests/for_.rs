use repetitive::repetitive;

repetitive! {
    @for [x, x_idx] in [['X, 0], ['Y, 1], ['Z, 2]], [y, y_idx] in [['X, 0], ['Y, 1], ['Z, 2]] {
        const @['_ x y]: u128 = @(x_idx * 10 + y_idx);
    }
}

const _: () = assert!(_XX == 00);
const _: () = assert!(_XY == 01);
const _: () = assert!(_XZ == 02);

const _: () = assert!(_YX == 10);
const _: () = assert!(_YY == 11);
const _: () = assert!(_YZ == 12);

const _: () = assert!(_ZX == 20);
const _: () = assert!(_ZY == 21);
const _: () = assert!(_ZZ == 22);
