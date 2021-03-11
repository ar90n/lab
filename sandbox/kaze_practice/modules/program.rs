use kaze::*;

pub fn lchika_program<'a>(c: &'a Context<'a>) {
    let lchika_program = c.module("LchikaProgram");
    let addr = lchika_program.input("addr", 4);

    let mem = lchika_program.mem("lchika", 4, 8);
    mem.initial_contents(&[0b10110011u32, 0b10110110u32, 0b10111100u32, 0b10111000u32, 0b10111000u32, 0b10111100u32, 0b10110110u32,
        0b10110011u32, 0b10110001u32, 0b11110000u32, 0b00000000u32, 0b00000000u32, 0b00000000u32, 0b00000000u32, 0b00000000u32, 0b00000000u32]);
   lchika_program.output("data", mem.read_port(addr, lchika_program.high()));
}

pub fn timer_program<'a>(c: &'a Context<'a>) {
    let lchika_program = c.module("TimerProgram");
    let addr = lchika_program.input("addr", 4);

    let mem = lchika_program.mem("timer", 4, 8);
    mem.initial_contents(&[
    0b10110111u32, 0b00000001u32, 0b11100001u32, 0b00000001u32, 0b11100011u32, 0b10110110u32, 0b00000001u32,
    0b11100110u32, 0b00000001u32, 0b11101000u32, 0b10110000u32, 0b10110100u32, 0b00000001u32, 0b11101010u32,
    0b10111000u32, 0b11111111u32,
    ]);
   lchika_program.output("data", mem.read_port(addr, lchika_program.high()));
}
