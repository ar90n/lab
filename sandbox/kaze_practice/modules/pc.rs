use kaze::*;

pub fn program_counter<'a>(c: &'a Context<'a>) -> &'a Module<'a> {
    let pc = c.module("ProgramCounter");

    let addr = pc.input("data", 4);
    let load_ = pc.input("load_", 1);

    let counter = pc.reg("counter", 4);
    counter.default_value(0x0u32);
    counter.drive_next(load_.mux(counter.value + pc.lit(1u32, 4), addr));
 
    pc.output("addr", counter.value);
    pc
}
