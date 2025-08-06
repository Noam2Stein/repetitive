use repetitive::repetitive;

repetitive! {
    @let n = 3;
    @let [start, end] = [0, n];
    @let range = start..end;

    @for i in range {
        @let name = ['A, 'B, 'C].index(i);
        @let name = @['_ @name];

        const @name: u8 = @i;
    }
}

const _: () = assert!(_A == 0);
const _: () = assert!(_B == 1);
const _: () = assert!(_C == 2);
