use repetitive::repetitive;

repetitive! {
    @let field_type = @{
        @if len > 1 {
            [u8; @len]
        } else {
            u8
        }
    };

    @let field = @{ @name: @field_type };

    struct _Triangle {
        @for [idx, name] in ['a, 'b, 'c, 'd].enumerate() {
            @let len = idx + 1;

            @field,
        }
    }
}

const _: _Triangle = _Triangle {
    a: 0,
    b: [0, 0],
    c: [0, 0, 0],
    d: [0, 0, 0, 0],
};

repetitive! {
    @let magic = @{
        @let val = 5;
        THREE
    };

    @let val = 3;
    const @magic: u8 = @val;

    const _: () = assert!(THREE == 3);
}
