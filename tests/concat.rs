use repetitive::repetitive;

repetitive! {
    const _: () = assert!(@(@["Hello" "World"] == 'HelloWorld));
    const _: () = assert!(@(@['Hello "World"] == 'HelloWorld));
    const _: () = assert!(@(@["Hello" '_ 'Worl 'd] == 'Hello_World));

    const _: () = assert!(@(@str["Hello" "World"] == "HelloWorld"));
    const _: () = assert!(@(@str['Hello "World"] == "HelloWorld"));
    const _: () = assert!(@(@str["Hello " 'Worl 'd] == "Hello World"));
}
