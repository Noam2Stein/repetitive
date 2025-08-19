use repetitive::repetitive;

repetitive! {
    const _: () = assert!(@(1.neg() == -1));
    const _: () = assert!(@(1.add(4) == 5));

    const _: () = assert!(@(1.min(2) == 1));
    const _: () = assert!(@(1.max(2) == 2));
    const _: () = assert!(@(1.clamp(2, 3) == 2));
    const _: () = assert!(@(5.clamp(2, 3) == 3));

    const _: () = assert!(@([1, 2, 3].index(1) == 2));
    const _: () = assert!(@([1, 2, 3].enumerate() == [[0, 1], [1, 2], [2, 3]]));
    const _: () = assert!(@([1, 2, 3].zip([4, 5, 6]) == [[1, 4], [2, 5], [3, 6]]));
    const _: () = assert!(@([1, 2, 3].chain([4, 5, 6]) == [1, 2, 3, 4, 5, 6]));
    const _: () = assert!(@([1, 2, 3].contains(2)));
    const _: () = assert!(!(@([1, 2, 3].contains(4))));
    const _: () = assert!(@([1, 2, 2, 1, 3, 3].unique() == [1, 2, 3]));

    const _: () = assert!(@(1.neg().add(-1) == -2));
    const _: () = assert!(@([1, 2][0].add(1) == 2));

    const _: () = assert!(@(1.to_float() == 1.0));
    const _: () = assert!(@(1.0.to_float() == 1.0));
    const _: () = assert!(@((-900).to_float() == -900.0));

    const _: () = assert!(@(1.4.round() == 1.0));
    const _: () = assert!(@(1.6.round() == 2.0));
    const _: () = assert!(@((-1.4).round() == -1.0));
    const _: () = assert!(@((-1.6).round() == -2.0));

    const _: () = assert!(@(1.4.floor() == 1.0));
    const _: () = assert!(@(1.6.floor() == 1.0));
    const _: () = assert!(@((-1.4).floor() == -2.0));
    const _: () = assert!(@((-1.6).floor() == -2.0));

    const _: () = assert!(@(1.4.ceil() == 2.0));
    const _: () = assert!(@(1.6.ceil() == 2.0));
    const _: () = assert!(@((-1.4).ceil() == -1.0));
    const _: () = assert!(@((-1.6).ceil() == -1.0));

    const _: () = assert!(@(1.4.trunc() == 1.0));
    const _: () = assert!(@(1.6.trunc() == 1.0));
    const _: () = assert!(@((-1.4).trunc() == -1.0));
    const _: () = assert!(@((-1.6).trunc() == -1.0));

    const _: () = assert!(@(1.4.atrunc() == 2.0));
    const _: () = assert!(@(1.6.atrunc() == 2.0));
    const _: () = assert!(@((-1.4).atrunc() == -2.0));
    const _: () = assert!(@((-1.6).atrunc() == -2.0));

    const _: () = assert!(@(1.4.iround() == 1));
    const _: () = assert!(@(1.6.iround() == 2));
    const _: () = assert!(@((-1.4).iround() == -1));
    const _: () = assert!(@((-1.6).iround() == -2));

    const _: () = assert!(@(1.4.ifloor() == 1));
    const _: () = assert!(@(1.6.ifloor() == 1));
    const _: () = assert!(@((-1.4).ifloor() == -2));
    const _: () = assert!(@((-1.6).ifloor() == -2));

    const _: () = assert!(@(1.4.iceil() == 2));
    const _: () = assert!(@(1.6.iceil() == 2));
    const _: () = assert!(@((-1.4).iceil() == -1));
    const _: () = assert!(@((-1.6).iceil() == -1));

    const _: () = assert!(@(1.4.itrunc() == 1));
    const _: () = assert!(@(1.6.itrunc() == 1));
    const _: () = assert!(@((-1.4).itrunc() == -1));
    const _: () = assert!(@((-1.6).itrunc() == -1));

    const _: () = assert!(@(1.4.iatrunc() == 2));
    const _: () = assert!(@(1.6.iatrunc() == 2));
    const _: () = assert!(@((-1.4).iatrunc() == -2));
    const _: () = assert!(@((-1.6).iatrunc() == -2));

    const _: () = assert!(@(1.0.log(2.0) == 0.0));
    const _: () = assert!(@(2.0.log(2.0) == 1.0));
    const _: () = assert!(@(4.0.log(2.0) == 2.0));
    const _: () = assert!(@(8.0.log(2.0) == 3.0));

    const _: () = assert!(@(1.0.log2() == 0.0));
    const _: () = assert!(@(2.0.log2() == 1.0));
    const _: () = assert!(@(4.0.log2() == 2.0));
    const _: () = assert!(@(8.0.log2() == 3.0));

    const _: () = assert!(@(1.0.log10() == 0.0));
    const _: () = assert!(@(10.0.log10() == 1.0));
    const _: () = assert!(@(100.0.log10() == 2.0));
    const _: () = assert!(@(1000.0.log10() == 3.0));
}
