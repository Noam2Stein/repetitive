use repetitive::repetitive;

repetitive! {
    const _: () = assert!(@(1.neg() == -1) == true);
    const _: () = assert!(@(1.add(4) == 5) == true);

    const _: () = assert!(@([1, 2, 3].index(1) == 2) == true);
    const _: () = assert!(@([1, 2, 3].enumerate() == [[0, 1], [1, 2], [2, 3]]) == true);
    const _: () = assert!(@([1, 2, 3].zip([4, 5, 6]) == [[1, 4], [2, 5], [3, 6]]) == true);
    const _: () = assert!(@([1, 2, 3].chain([4, 5, 6]) == [1, 2, 3, 4, 5, 6]) == true);

    const _: () = assert!(@(1.neg().add(-1) == -2) == true);
    const _: () = assert!(@([1, 2][0].add(1) == 2) == true);
}
