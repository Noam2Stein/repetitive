use repetitive::repetitive;

repetitive! {
    @let frag = true || false;
    const _: () = assert!(@frag == true);

    @let frag = true && false;
    const _: () = assert!(@frag == false);

    @let frag = true ^ false;
    const _: () = assert!(@frag == true);

    @let frag = true | false;
    const _: () = assert!(@frag == true);

    @let frag = true & false;
    const _: () = assert!(@frag == false);

    @let frag = true == false;
    const _: () = assert!(@frag == false);

    @let frag = true != false;
    const _: () = assert!(@frag == true);

    @let frag = 1 < 2;
    const _: () = assert!(@frag == true);

    @let frag = 1 > 2;
    const _: () = assert!(@frag == false);

    @let frag = 1 <= 2;
    const _: () = assert!(@frag == true);

    @let frag = 1 >= 2;
    const _: () = assert!(@frag == false);

    @let frag = 1 + 2;
    const _: () = assert!(@frag == 3);

    @let frag = 1 - 2;
    const _: () = assert!(@frag == -1);

    @let frag = 1 * 2;
    const _: () = assert!(@frag == 2);

    @let frag = 1 / 2;
    const _: () = assert!(@frag == 0);

    @let frag = 1 % 2;
    const _: () = assert!(@frag == 1);

    @let frag = 1..3;
    const _: () = assert!([@for n in frag { @n, }].len() == 2);

    @let frag = 1..=3;
    const _: () = assert!([@for n in frag { @n, }].len() == 3);

    @let frag = !true;
    const _: () = assert!(@frag == false);

    @let frag = -(1);
    const _: () = assert!(@frag == -1);

}
