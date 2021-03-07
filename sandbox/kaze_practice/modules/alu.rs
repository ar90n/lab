use kaze::*;

pub fn alu<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let alu = c.module("ALU");

    let a = alu.lit(0u32, 1).concat(alu.input("A", 4));
    let b = alu.lit(0u32, 1).concat(alu.input("B", 4));
    let sum = a + b;
    alu.output("sum", sum.bits(3, 0));
    alu.output("carry", sum.bit(4));

    return alu;
}