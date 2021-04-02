use kaze::*;

fn create_memory<'a>(m: &'a Module<'a>, addr: &'a Signal<'a>, data: &[u32; 16]) -> &'a Signal<'a> {
    if_(addr.eq(m.lit(0x0u32,4)), m.lit(data[0x0], 8))
    .else_if(addr.eq(m.lit(0x1u32,4)), m.lit(data[0x1], 8))
    .else_if(addr.eq(m.lit(0x2u32,4)), m.lit(data[0x2], 8))
    .else_if(addr.eq(m.lit(0x3u32,4)), m.lit(data[0x3], 8))
    .else_if(addr.eq(m.lit(0x4u32,4)), m.lit(data[0x4], 8))
    .else_if(addr.eq(m.lit(0x5u32,4)), m.lit(data[0x5], 8))
    .else_if(addr.eq(m.lit(0x6u32,4)), m.lit(data[0x6], 8))
    .else_if(addr.eq(m.lit(0x7u32,4)), m.lit(data[0x7], 8))
    .else_if(addr.eq(m.lit(0x8u32,4)), m.lit(data[0x8], 8))
    .else_if(addr.eq(m.lit(0x9u32,4)), m.lit(data[0x9], 8))
    .else_if(addr.eq(m.lit(0xau32,4)), m.lit(data[0xa], 8))
    .else_if(addr.eq(m.lit(0xbu32,4)), m.lit(data[0xb], 8))
    .else_if(addr.eq(m.lit(0xcu32,4)), m.lit(data[0xc], 8))
    .else_if(addr.eq(m.lit(0xdu32,4)), m.lit(data[0xd], 8))
    .else_if(addr.eq(m.lit(0xeu32,4)), m.lit(data[0xe], 8))
    .else_(m.lit(data[0xf], 8))
}

pub fn memory<'a>(c: &'a Context<'a>) {
    let memory = c.module("Memory");

    let lchika_program: [u32; 16] = [
        0b10110011,
        0b10110110,
        0b10111100,
        0b10111000,
        0b10111000,
        0b10111100,
        0b10110110,
        0b10110011,
        0b10110001,
        0b11110000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
    ];

    let addr = memory.input("addr", 4);
    let data = create_memory(memory, addr, &lchika_program);
    memory.output("data", data);
}
