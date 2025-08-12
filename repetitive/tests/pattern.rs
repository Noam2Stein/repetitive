use repetitive::repetitive;

repetitive! {
    @let [a, b] = [1, 2];
    @let [1, b] = [1, 2];
}
