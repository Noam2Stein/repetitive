use repetitive::repetitive;

repetitive! {
    @let n = 3;
    @let [start, end] = [0, n];
    @let range = start..end;

    @let underscore = '_;
    @let names = ['A, 'B, 'C];

    @for i in range {
        @let name = names[i];
        @let name = @[underscore name];

        const @name: u8 = @i;
    }
}

const _: () = assert!(_A == 0);
const _: () = assert!(_B == 1);
const _: () = assert!(_C == 2);
