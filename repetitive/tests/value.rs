use repetitive::repetitive;

repetitive! {
    const _: () = assert!(@(1) == 1);
    const _: () = assert!(@(-1) == -1);

    const _: () = assert!(@(1.0) == 1.0);
    const _: () = assert!(@(-1.0) == -1.0);

    const _: () = assert!(@(true) == true);
    const _: () = assert!(@(false) == false);

    const _: &str = @("1"); // str cmp isnt const
    const _: () = assert!(@('1') == '1');

    const _: () = assert!(@{ 1 } == 1);
}
