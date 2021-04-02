pub struct Decoder{
    // Inputs
    pub carry: bool, // 1 bit(s)
    pub op: u32, // 4 bit(s)
    // Outputs
    pub load: u32, // 4 bit(s)
    pub select: u32, // 2 bit(s)
}

#[allow(unused_parens)]
#[automatically_derived]
impl Decoder {
    pub fn new() -> Decoder {
        Decoder {
            // Inputs
            carry: false, // 1 bit(s)
            op: 0, // 4 bit(s)
            // Outputs
            load: 0, // 4 bit(s)
            select: 0, // 2 bit(s)
        }
    }

    pub fn prop(&mut self) {
        let __temp_0 = (self.op & 0xfu32);
        self.load = (((((!(((__temp_0 >> 0x3u32) & 0x1u32) != 0x0u32) | !(((__temp_0 >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((__temp_0 & 0x1u32) != 0x0u32) & self.carry)) as u32) << 0x3u32) | (((!(!(((__temp_0 >> 0x2u32) & 0x1u32) != 0x0u32) & (((__temp_0 >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((__temp_0 >> 0x2u32) & 0x1u32) != 0x0u32) | (((__temp_0 >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((__temp_0 >> 0x2u32) & 0x1u32) != 0x0u32) | (((__temp_0 >> 0x3u32) & 0x1u32) != 0x0u32)) as u32))));
        self.select = ((((((__temp_0 >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((__temp_0 & 0x1u32) != 0x0u32) | (((__temp_0 >> 0x3u32) & 0x1u32) != 0x0u32)) as u32));
    }
}

