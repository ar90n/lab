use kaze::*;

pub fn program_counter<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let pc = c.module("ProgramCounter");

    let addr = pc.input("addr", 4);
    let ld = pc.input("load_", 1);

    let counter = pc.reg("counter", 4);
    counter.default_value(0x0u32);
    counter.drive_next(ld.mux(counter.value + pc.lit(1u32, 4), addr));
 
    pc.output("output", counter.value);
    pc
}
