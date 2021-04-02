pub struct ProgramCounter{
    // Inputs
    pub data: u32, // 4 bit(s)
    pub load_: bool, // 1 bit(s)
    // Outputs
    pub addr: u32, // 4 bit(s)

    // Regs
    __reg_counter_0: u32, // 4 bit(s)
    __reg_counter_0_next: u32,
}

#[allow(unused_parens)]
#[automatically_derived]
impl ProgramCounter {
    pub fn new() -> ProgramCounter {
        ProgramCounter {
            // Inputs
            data: 0, // 4 bit(s)
            load_: false, // 1 bit(s)
            // Outputs
            addr: 0, // 4 bit(s)

            // Regs
            __reg_counter_0: 0, // 4 bit(s)
            __reg_counter_0_next: 0,
        }
    }

    pub fn reset(&mut self) {
        self.__reg_counter_0 = 0x0u32;
    }

    pub fn posedge_clk(&mut self) {
        self.__reg_counter_0 = self.__reg_counter_0_next;
    }

    pub fn prop(&mut self) {
        self.addr = self.__reg_counter_0;
        self.__reg_counter_0_next = if self.load_ { (self.__reg_counter_0.wrapping_add(0x1u32) & 0xfu32) } else { (self.data & 0xfu32)};
    }
}

