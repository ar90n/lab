pub struct Register{
    // Inputs
    pub data: u32, // 4 bit(s)
    pub load_: bool, // 1 bit(s)
    // Outputs
    pub value: u32, // 4 bit(s)

    // Regs
    __reg_reg_0: u32, // 4 bit(s)
    __reg_reg_0_next: u32,
}

#[allow(unused_parens)]
#[automatically_derived]
impl Register {
    pub fn new() -> Register {
        Register {
            // Inputs
            data: 0, // 4 bit(s)
            load_: false, // 1 bit(s)
            // Outputs
            value: 0, // 4 bit(s)

            // Regs
            __reg_reg_0: 0, // 4 bit(s)
            __reg_reg_0_next: 0,
        }
    }

    pub fn reset(&mut self) {
        self.__reg_reg_0 = 0x0u32;
    }

    pub fn posedge_clk(&mut self) {
        self.__reg_reg_0 = self.__reg_reg_0_next;
    }

    pub fn prop(&mut self) {
        self.value = self.__reg_reg_0;
        self.__reg_reg_0_next = if self.load_ { self.__reg_reg_0 } else { (self.data & 0xfu32)};
    }
}

