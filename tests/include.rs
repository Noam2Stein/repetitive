use repetitive::repetitive;

repetitive! {
    const _: () = assert!(@(include("tests/include_helper.txt")) == 19);
    const _: () = assert!(@(-include("tests/include_helper.txt")) == -19);
    const _: () = assert!(@(include("tests/include_helper.txt") * 2) == 38);
}
