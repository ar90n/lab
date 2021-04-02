pub struct Memory{
    // Inputs
    pub addr: u32, // 4 bit(s)
    // Outputs
    pub data: u32, // 8 bit(s)
}

#[allow(unused_parens)]
#[automatically_derived]
impl Memory {
    pub fn new() -> Memory {
        Memory {
            // Inputs
            addr: 0, // 4 bit(s)
            // Outputs
            data: 0, // 8 bit(s)
        }
    }

    pub fn prop(&mut self) {
        let __temp_0 = (self.addr & 0xfu32);
        self.data = if (__temp_0 == 0x0u32) { 0xb3u32 } else { if (__temp_0 == 0x1u32) { 0xb6u32 } else { if (__temp_0 == 0x2u32) { 0xbcu32 } else { if (__temp_0 == 0x3u32) { 0xb8u32 } else { if (__temp_0 == 0x4u32) { 0xb8u32 } else { if (__temp_0 == 0x5u32) { 0xbcu32 } else { if (__temp_0 == 0x6u32) { 0xb6u32 } else { if (__temp_0 == 0x7u32) { 0xb3u32 } else { if (__temp_0 == 0x8u32) { 0xb1u32 } else { if (__temp_0 == 0x9u32) { 0xf0u32 } else { if (__temp_0 == 0xau32) { 0x0u32 } else { if (__temp_0 == 0xbu32) { 0x0u32 } else { if (__temp_0 == 0xcu32) { 0x0u32 } else { if (__temp_0 == 0xdu32) { 0x0u32 } else { if (__temp_0 == 0xeu32) { 0x0u32 } else { 0x0u32}}}}}}}}}}}}}}};
    }
}

