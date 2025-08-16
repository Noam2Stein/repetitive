use repetitive::repetitive;

repetitive! {
    repetitive! {
        const @@(~@['FU "N"]): u8 = 1;
    }
}

const _: () = assert!(FUN == 1);
