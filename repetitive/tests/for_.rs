use repetitive::repetitive;

repetitive! {
    @for [x, x_idx] in [['X, 0], ['Y, 1], ['Z, 2]], [y, y_idx] in [['X, x_idx + 0], ['Y, x_idx + 1], ['Z, x_idx + 2]] {
        const @['_ x y]: u128 = @(x_idx * 10 + y_idx);
    }
}

const _: () = assert!(_XX == 00);
const _: () = assert!(_XY == 01);
const _: () = assert!(_XZ == 02);

const _: () = assert!(_YX == 11);
const _: () = assert!(_YY == 12);
const _: () = assert!(_YZ == 13);

const _: () = assert!(_ZX == 22);
const _: () = assert!(_ZY == 23);
const _: () = assert!(_ZZ == 24);
