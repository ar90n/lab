pub struct TD4{
    // Inputs
    pub data: u32, // 8 bit(s)
    pub in_: u32, // 4 bit(s)
    // Outputs
    pub addr: u32, // 4 bit(s)
    pub out: u32, // 4 bit(s)

    // Regs
    __reg_reg_3: u32, // 4 bit(s)
    __reg_reg_3_next: u32,
    __reg_reg_4: u32, // 4 bit(s)
    __reg_reg_4_next: u32,
    __reg_reg_1: u32, // 4 bit(s)
    __reg_reg_1_next: u32,
    __reg_carry_2: bool, // 1 bit(s)
    __reg_carry_2_next: bool,
    __reg_counter_0: u32, // 4 bit(s)
    __reg_counter_0_next: u32,
}

#[allow(unused_parens)]
#[automatically_derived]
impl TD4 {
    pub fn new() -> TD4 {
        TD4 {
            // Inputs
            data: 0, // 8 bit(s)
            in_: 0, // 4 bit(s)
            // Outputs
            addr: 0, // 4 bit(s)
            out: 0, // 4 bit(s)

            // Regs
            __reg_reg_3: 0, // 4 bit(s)
            __reg_reg_3_next: 0,
            __reg_reg_4: 0, // 4 bit(s)
            __reg_reg_4_next: 0,
            __reg_reg_1: 0, // 4 bit(s)
            __reg_reg_1_next: 0,
            __reg_carry_2: false, // 1 bit(s)
            __reg_carry_2_next: false,
            __reg_counter_0: 0, // 4 bit(s)
            __reg_counter_0_next: 0,
        }
    }

    pub fn reset(&mut self) {
        self.__reg_reg_3 = 0x0u32;
        self.__reg_reg_4 = 0x0u32;
        self.__reg_reg_1 = 0x0u32;
        self.__reg_carry_2 = false;
        self.__reg_counter_0 = 0x0u32;
    }

    pub fn posedge_clk(&mut self) {
        self.__reg_reg_3 = self.__reg_reg_3_next;
        self.__reg_reg_4 = self.__reg_reg_4_next;
        self.__reg_reg_1 = self.__reg_reg_1_next;
        self.__reg_carry_2 = self.__reg_carry_2_next;
        self.__reg_counter_0 = self.__reg_counter_0_next;
    }

    pub fn prop(&mut self) {
        self.addr = self.__reg_counter_0;
        self.out = self.__reg_reg_4;
        let __temp_0 = (self.data & 0xffu32);
        let __temp_1 = ((((false as u32) << 0x4u32) | if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x0u32) { self.__reg_reg_3 } else { if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x1u32) { self.__reg_reg_1 } else { if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x2u32) { (self.in_ & 0xfu32) } else { 0x0u32}}}).wrapping_add((((false as u32) << 0x4u32) | (__temp_0 & 0xfu32))) & 0x1fu32);
        self.__reg_reg_3_next = if (((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) & 0x1u32) != 0x0u32) { self.__reg_reg_3 } else { (__temp_1 & 0xfu32)};
        self.__reg_reg_4_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x2u32) & 0x1u32) != 0x0u32) { self.__reg_reg_4 } else { (__temp_1 & 0xfu32)};
        self.__reg_reg_1_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x1u32) & 0x1u32) != 0x0u32) { self.__reg_reg_1 } else { (__temp_1 & 0xfu32)};
        self.__reg_carry_2_next = (((__temp_1 >> 0x4u32) & 0x1u32) != 0x0u32);
        self.__reg_counter_0_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x3u32) & 0x1u32) != 0x0u32) { (self.__reg_counter_0.wrapping_add(0x1u32) & 0xfu32) } else { (__temp_1 & 0xfu32)};
    }
}

