use kaze::*;

pub fn selector<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let selector = c.module("Selector");

    let select = selector.input("select", 2);
    let input_0 = selector.input("input_0", 4);
    let input_1 = selector.input("input_1", 4);
    let input_2 = selector.input("input_2", 4);
    let input_3 = selector.input("input_3", 4);

    let value = select.bit(1).mux(
        select.bit(0).mux(input_3, input_2),
        select.bit(0).mux(input_1, input_0),
    );
    selector.output("output", value);

    selector
}
