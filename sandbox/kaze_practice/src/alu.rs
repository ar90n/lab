pub struct ALU{
    // Inputs
    pub A: u32, // 4 bit(s)
    pub B: u32, // 4 bit(s)
    // Outputs
    pub carry: bool, // 1 bit(s)
    pub sum: u32, // 4 bit(s)
}

#[allow(unused_parens)]
#[automatically_derived]
impl ALU {
    pub fn new() -> ALU {
        ALU {
            // Inputs
            A: 0, // 4 bit(s)
            B: 0, // 4 bit(s)
            // Outputs
            carry: false, // 1 bit(s)
            sum: 0, // 4 bit(s)
        }
    }

    pub fn prop(&mut self) {
        let __temp_0 = ((((false as u32) << 0x4u32) | (self.A & 0xfu32)).wrapping_add((((false as u32) << 0x4u32) | (self.B & 0xfu32))) & 0x1fu32);
        self.carry = (((__temp_0 >> 0x4u32) & 0x1u32) != 0x0u32);
        self.sum = (__temp_0 & 0xfu32);
    }
}

