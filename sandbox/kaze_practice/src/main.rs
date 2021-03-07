mod alu;
mod decoder;
mod pc;
mod register;
mod selector;
mod td4;

fn main() {}

#[test]
fn test_decoder() {
    let mut decoder = decoder::Decoder::new();
    decoder.carry = false;

    // MOV A, Im
    decoder.op = 0x3;
    decoder.prop();
    assert_eq!(decoder.select, 0b11);
    assert_eq!(decoder.load, 0b1110);

    // MOV B, Im
    decoder.op = 0x7;
    decoder.prop();
    assert_eq!(decoder.select, 0b11);
    assert_eq!(decoder.load, 0b1101);

    // MOV A, B
    decoder.op = 0x1;
    decoder.prop();
    assert_eq!(decoder.select, 0b01);
    assert_eq!(decoder.load, 0b1110);

    // MOV B, A
    decoder.op = 0x4;
    decoder.prop();
    assert_eq!(decoder.select, 0b00);
    assert_eq!(decoder.load, 0b1101);

    // ADD A, Im
    decoder.op = 0x0;
    decoder.prop();
    assert_eq!(decoder.select, 0b00);
    assert_eq!(decoder.load, 0b1110);

    // ADD B, Im
    decoder.op = 0x5;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b01);
    assert_eq!(decoder.load, 0b1101);

    // IN A
    decoder.op = 0x2;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b10);
    assert_eq!(decoder.load, 0b1110);

    // IN B
    decoder.op = 0x6;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b10);
    assert_eq!(decoder.load, 0b1101);

    // OUT Im
    decoder.op = 0xb;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b11);
    assert_eq!(decoder.load, 0b1011);

    // OUT B
    decoder.op = 0x9;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b01);
    assert_eq!(decoder.load, 0b1011);

    // JMP Im
    decoder.op = 0xf;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b11);
    assert_eq!(decoder.load, 0b0111);

    // JNC Im
    decoder.op = 0xe;
    decoder.carry = false;
    decoder.prop();
    assert_eq!(decoder.select, 0b11);
    assert_eq!(decoder.load, 0b0111);
}

#[test]
fn test_pc() {
    let mut pc = pc::ProgramCounter::new();
    pc.load_ = true;

    pc.reset();
    pc.prop();

    assert_eq!(pc.addr, 0x0u32);

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0x1u32);

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0x2u32);

    pc.load_ = false;
    pc.data = 0xe;

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0x3u32);

    pc.load_ = true;

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0xeu32);

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0xfu32);

    pc.posedge_clk();
    pc.prop();

    assert_eq!(pc.addr, 0x0u32);
}

#[test]
fn test_alu() {
    let mut alu = alu::ALU::new();

    alu.A = 0x1;
    alu.B = 0x2;
    alu.prop();

    assert_eq!(alu.sum, 0x3);
    assert_eq!(alu.carry, false);

    alu.A = 0x8;
    alu.B = 0x9;
    alu.prop();

    assert_eq!(alu.sum, 0x1);
    assert_eq!(alu.carry, true);
}

#[test]
fn test_register() {
    let mut register = register::Register::new();

    register.reset();

    register.load_ = false;
    register.data = 0xe;
    register.prop();

    assert_eq!(register.value, 0x0);

    register.posedge_clk();
    register.prop();

    assert_eq!(register.value, 0xe);
    register.load_ = true;

    register.posedge_clk();
    register.prop();

    assert_eq!(register.value, 0xe);
}

#[test]
fn test_selector() {
    let mut selector = selector::Selector::new();

    selector.input_0 = 0x1;
    selector.input_1 = 0x2;
    selector.input_2 = 0x3;
    selector.input_3 = 0x4;
    selector.select = 0x0;
    selector.prop();

    assert_eq!(selector.value, 0x1);

    selector.select = 0x1;
    selector.prop();

    assert_eq!(selector.value, 0x2);

    selector.select = 0x2;
    selector.prop();

    assert_eq!(selector.value, 0x3);

    selector.select = 0x3;
    selector.prop();

    assert_eq!(selector.value, 0x4);
}

#[test]
fn test_td4() {
    let mut td4 = td4::TD4::new();
    let memory: Vec<u32> = vec![
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
    ];

    td4.reset();
    td4.prop();
    assert_eq!(td4.out, 0b0000u32);
    assert_eq!(td4.addr, 0b0000u32);

    td4.data = memory[td4.addr as usize];
    td4.in_ = 0x0u32;
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0011u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0110u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b1100u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b1000u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b1000u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b1100u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0110u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0011u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0001u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0001u32);

    td4.data = memory[td4.addr as usize];
    td4.prop();
    td4.posedge_clk();
    td4.prop();

    assert_eq!(td4.out, 0b0011u32);
}
