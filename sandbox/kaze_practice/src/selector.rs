pub struct Selector{
    // Inputs
    pub input_0: u32, // 4 bit(s)
    pub input_1: u32, // 4 bit(s)
    pub input_2: u32, // 4 bit(s)
    pub input_3: u32, // 4 bit(s)
    pub select: u32, // 2 bit(s)
    // Outputs
    pub value: u32, // 4 bit(s)
}

#[allow(unused_parens)]
#[automatically_derived]
impl Selector {
    pub fn new() -> Selector {
        Selector {
            // Inputs
            input_0: 0, // 4 bit(s)
            input_1: 0, // 4 bit(s)
            input_2: 0, // 4 bit(s)
            input_3: 0, // 4 bit(s)
            select: 0, // 2 bit(s)
            // Outputs
            value: 0, // 4 bit(s)
        }
    }

    pub fn prop(&mut self) {
        let __temp_0 = (self.select & 0x3u32);
        self.value = if (__temp_0 == 0x0u32) { (self.input_0 & 0xfu32) } else { if (__temp_0 == 0x1u32) { (self.input_1 & 0xfu32) } else { if (__temp_0 == 0x2u32) { (self.input_2 & 0xfu32) } else { (self.input_3 & 0xfu32)}}};
    }
}

