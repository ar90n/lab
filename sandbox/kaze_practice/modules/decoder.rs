use kaze::*;

pub fn decoder<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let decoder = c.module("Decoder");

    let op = decoder.input("op", 4);
    let carry = decoder.input("carry", 1);

    let select_b = op.bit(1);
    let select_a = op.bit(0) | op.bit(3);

    let load_0_ = op.bit(2) | op.bit(3);
    let load_1_ = !op.bit(2) | op.bit(3);
    let load_2_ = !(!op.bit(2) & op.bit(3));
    let load_3_ = !op.bit(3) | !op.bit(2) | (!op.bit(0) & carry);

    decoder.output("select", select_b.concat(select_a));
    decoder.output(
        "load",
        load_3_.concat(load_2_.concat(load_1_.concat(load_0_))),
    );

    decoder
}
