use kaze::*;

pub fn selector<'a>(c: &'a Context<'a>) {
    let selector = c.module("Selector");

    let select = selector.input("select", 2);
    let input_0 = selector.input("input_0", 4);
    let input_1 = selector.input("input_1", 4);
    let input_2 = selector.input("input_2", 4);
    let input_3 = selector.input("input_3", 4);

    let value = if_(select.eq(selector.lit(0b00u32,2)),input_0
    ).else_if(select.eq(selector.lit(0b01u32,2)),input_1
    ).else_if(select.eq(selector.lit(0b10u32,2)),input_2
    ).else_(input_3);
    selector.output("value", value);
}
