use repetitive::repetitive;

repetitive! {
    const _: () = assert!(_str_eq(stringify!(@["Hello" "World"]), "HelloWorld"));
    const _: () = assert!(_str_eq(stringify!(@['Hello "World"]), "HelloWorld"));
    const _: () = assert!(_str_eq(stringify!(@["Hello" 'Worl 'd]), "HelloWorld"));

    const _: () = assert!(_str_eq(@str["Hello" "World"], "HelloWorld"));
    const _: () = assert!(_str_eq(@str['Hello "World"], "HelloWorld"));
    const _: () = assert!(_str_eq(@str["Hello " 'Worl 'd'], "Hello World"));
}

const fn _str_eq(left: &str, right: &str) -> bool {
    let left = left.as_bytes();
    let right = right.as_bytes();

    if left.len() != right.len() {
        return false;
    }

    let mut i = 0;
    while i < left.len() {
        if left[i] != right[i] {
            return false;
        }

        i += 1;
    }

    true
}
