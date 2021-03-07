mod decoder;
mod register;

fn main() {
}

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
