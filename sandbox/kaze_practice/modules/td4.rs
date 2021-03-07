use kaze::*;

pub fn td4<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let td4 = c.module("TD4");

    let data = td4.input("data", 8);
    let op = data.bits(7, 4);
    let immediate = data.bits(3, 0);

    let in_ = td4.input("in_", 4);

    let reg_a = td4.instance("reg_a", "Register");
    let reg_b = td4.instance("reg_b", "Register");
    let reg_out = td4.instance("reg_out", "Register");
    let pc = td4.instance("pc", "ProgramCounter");

    let decoder = td4.instance("decoder", "Decoder");
    let select = decoder.output("select");
    let load = decoder.output("load");
    decoder.drive_input("op", op);

    let selector = td4.instance("selector", "Selector");
    let sel_value = selector.output("value");
    selector.drive_input("input_0", reg_a.output("value"));
    selector.drive_input("input_1", reg_b.output("value"));
    selector.drive_input("input_2", in_);
    selector.drive_input("input_3", td4.lit(0x0u32, 4));
    selector.drive_input("select", select);

    let alu = td4.instance("alu", "ALU");
    let sum = alu.output("sum");
    alu.drive_input("A", sel_value);
    alu.drive_input("B", immediate);

    reg_a.drive_input("data", sum);
    reg_a.drive_input("load_", load.bit(0));

    reg_b.drive_input("data", sum);
    reg_b.drive_input("load_", load.bit(1));

    reg_out.drive_input("data", sum);
    reg_out.drive_input("load_", load.bit(2));

    pc.drive_input("data", sum);
    pc.drive_input("load_", load.bit(3));

    let carry = alu.output("carry").reg_next_with_default("carry", false);
    decoder.drive_input("carry", carry);

    td4.output("out", reg_out.output("value"));
    td4.output("addr", pc.output("addr"));

    td4
}
