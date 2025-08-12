use repetitive::repetitive;

repetitive! {
    @let [a, b] = [1, 2];
    @let [[1, 2], b] = [[1, 2], 2];
    @let [_, _] = [1, 2];
}
