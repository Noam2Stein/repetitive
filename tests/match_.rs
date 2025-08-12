use repetitive::repetitive;

repetitive! {
    @for uint in ['u8, 'u16, 'u32, 'u64, 'u128] {
        @let size = match uint {
            'u8 => 8,
            'u16 => 16,
            'u32 => 32,
            'u64 => 64,
            'u128 => 128,
        };

        @let UINT = match uint {
            'u8 => 'U8,
            'u16 => 'U16,
            'u32 => 'U32,
            'u64 => 'U64,
            'u128 => 'U128,
        };

        const @[UINT '_SIZE]: @uint = @size;
    }
}

const _: () = assert!(U8_SIZE == 8);
const _: () = assert!(U16_SIZE == 16);
const _: () = assert!(U32_SIZE == 32);
const _: () = assert!(U64_SIZE == 64);
const _: () = assert!(U128_SIZE == 128);
