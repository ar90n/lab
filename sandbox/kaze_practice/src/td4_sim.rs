pub struct TD4<T: kaze::runtime::tracing::Trace>{
    // Inputs
    pub data: u32, // 8 bit(s)
    pub in_: u32, // 4 bit(s)
    // Outputs
    pub addr: u32, // 4 bit(s)
    pub out: u32, // 4 bit(s)

    // Regs
    __reg_counter_0: u32, // 4 bit(s)
    __reg_counter_0_next: u32,
    __reg_carry_2: bool, // 1 bit(s)
    __reg_carry_2_next: bool,
    __reg_reg_4: u32, // 4 bit(s)
    __reg_reg_4_next: u32,
    __reg_reg_1: u32, // 4 bit(s)
    __reg_reg_1_next: u32,
    __reg_reg_3: u32, // 4 bit(s)
    __reg_reg_3_next: u32,

    __trace: T,
    __trace_signal_id_reg_8: T::SignalId,
    __trace_signal_id_data_0: T::SignalId,
    __trace_signal_id_in__1: T::SignalId,
    __trace_signal_id_addr_2: T::SignalId,
    __trace_signal_id_out_3: T::SignalId,
    __trace_signal_id_carry_5: T::SignalId,
    __trace_signal_id_counter_4: T::SignalId,
    __trace_signal_id_reg_7: T::SignalId,
    __trace_signal_id_reg_6: T::SignalId,
}

#[allow(unused_parens)]
#[automatically_derived]
impl<T: kaze::runtime::tracing::Trace> TD4<T> {
    pub fn new(instance_name: &'static str, mut trace: T) -> std::io::Result<TD4<T>> {
        trace.push_module(instance_name)?;
        let __trace_signal_id_data_0 = trace.add_signal("data", 8, kaze::runtime::tracing::TraceValueType::U32)?;
        let __trace_signal_id_in__1 = trace.add_signal("in_", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        let __trace_signal_id_addr_2 = trace.add_signal("addr", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        let __trace_signal_id_out_3 = trace.add_signal("out", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        let __trace_signal_id_carry_5 = trace.add_signal("carry", 1, kaze::runtime::tracing::TraceValueType::Bool)?;
        trace.push_module("decoder")?;
        trace.pop_module()?;
        trace.push_module("reg_a")?;
        let __trace_signal_id_reg_8 = trace.add_signal("reg", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        trace.pop_module()?;
        trace.push_module("reg_b")?;
        let __trace_signal_id_reg_7 = trace.add_signal("reg", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        trace.pop_module()?;
        trace.push_module("reg_out")?;
        let __trace_signal_id_reg_6 = trace.add_signal("reg", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        trace.pop_module()?;
        trace.push_module("alu")?;
        trace.pop_module()?;
        trace.push_module("pc")?;
        let __trace_signal_id_counter_4 = trace.add_signal("counter", 4, kaze::runtime::tracing::TraceValueType::U32)?;
        trace.pop_module()?;
        trace.push_module("selector")?;
        trace.pop_module()?;
        trace.pop_module()?;

        Ok(TD4 {
            // Inputs
            data: 0, // 8 bit(s)
            in_: 0, // 4 bit(s)
            // Outputs
            addr: 0, // 4 bit(s)
            out: 0, // 4 bit(s)

            // Regs
            __reg_counter_0: 0, // 4 bit(s)
            __reg_counter_0_next: 0,
            __reg_carry_2: false, // 1 bit(s)
            __reg_carry_2_next: false,
            __reg_reg_4: 0, // 4 bit(s)
            __reg_reg_4_next: 0,
            __reg_reg_1: 0, // 4 bit(s)
            __reg_reg_1_next: 0,
            __reg_reg_3: 0, // 4 bit(s)
            __reg_reg_3_next: 0,

            __trace: trace,
            __trace_signal_id_reg_8,
            __trace_signal_id_data_0,
            __trace_signal_id_in__1,
            __trace_signal_id_addr_2,
            __trace_signal_id_out_3,
            __trace_signal_id_carry_5,
            __trace_signal_id_counter_4,
            __trace_signal_id_reg_7,
            __trace_signal_id_reg_6,
        })
    }

    pub fn reset(&mut self) {
        self.__reg_counter_0 = 0x0u32;
        self.__reg_carry_2 = false;
        self.__reg_reg_4 = 0x0u32;
        self.__reg_reg_1 = 0x0u32;
        self.__reg_reg_3 = 0x0u32;
    }

    pub fn posedge_clk(&mut self) {
        self.__reg_counter_0 = self.__reg_counter_0_next;
        self.__reg_carry_2 = self.__reg_carry_2_next;
        self.__reg_reg_4 = self.__reg_reg_4_next;
        self.__reg_reg_1 = self.__reg_reg_1_next;
        self.__reg_reg_3 = self.__reg_reg_3_next;
    }

    pub fn prop(&mut self) {
        self.addr = self.__reg_counter_0;
        self.out = self.__reg_reg_4;
        let __temp_0 = (self.data & 0xffu32);
        let __temp_1 = ((((false as u32) << 0x4u32) | if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x0u32) { self.__reg_reg_3 } else { if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x1u32) { self.__reg_reg_1 } else { if (((((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x1u32) & 0x1u32) != 0x0u32) as u32) << 0x1u32) | ((((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)) == 0x2u32) { (self.in_ & 0xfu32) } else { 0x0u32}}}).wrapping_add((((false as u32) << 0x4u32) | (__temp_0 & 0xfu32))) & 0x1fu32);
        self.__reg_counter_0_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x3u32) & 0x1u32) != 0x0u32) { (self.__reg_counter_0.wrapping_add(0x1u32) & 0xfu32) } else { (__temp_1 & 0xfu32)};
        self.__reg_carry_2_next = (((__temp_1 >> 0x4u32) & 0x1u32) != 0x0u32);
        self.__reg_reg_4_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x2u32) & 0x1u32) != 0x0u32) { self.__reg_reg_4 } else { (__temp_1 & 0xfu32)};
        self.__reg_reg_1_next = if ((((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) >> 0x1u32) & 0x1u32) != 0x0u32) { self.__reg_reg_1 } else { (__temp_1 & 0xfu32)};
        self.__reg_reg_3_next = if (((((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32) | !(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32)) | (!((((__temp_0 >> 0x4u32) & 0xfu32) & 0x1u32) != 0x0u32) & self.__reg_carry_2)) as u32) << 0x3u32) | (((!(!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) & (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x2u32) | ((((!(((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32) << 0x1u32) | (((((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x2u32) & 0x1u32) != 0x0u32) | (((((__temp_0 >> 0x4u32) & 0xfu32) >> 0x3u32) & 0x1u32) != 0x0u32)) as u32)))) & 0x1u32) != 0x0u32) { self.__reg_reg_3 } else { (__temp_1 & 0xfu32)};
    }

    pub fn update_trace(&mut self, time_stamp: u64) -> std::io::Result<()> {
        self.__trace.update_time_stamp(time_stamp)?;

        self.__trace.update_signal(&self.__trace_signal_id_reg_8, kaze::runtime::tracing::TraceValue::U32(self.__reg_reg_3))?;
        self.__trace.update_signal(&self.__trace_signal_id_data_0, kaze::runtime::tracing::TraceValue::U32(self.data))?;
        self.__trace.update_signal(&self.__trace_signal_id_in__1, kaze::runtime::tracing::TraceValue::U32(self.in_))?;
        self.__trace.update_signal(&self.__trace_signal_id_addr_2, kaze::runtime::tracing::TraceValue::U32(self.addr))?;
        self.__trace.update_signal(&self.__trace_signal_id_out_3, kaze::runtime::tracing::TraceValue::U32(self.out))?;
        self.__trace.update_signal(&self.__trace_signal_id_carry_5, kaze::runtime::tracing::TraceValue::Bool(self.__reg_carry_2))?;
        self.__trace.update_signal(&self.__trace_signal_id_counter_4, kaze::runtime::tracing::TraceValue::U32(self.__reg_counter_0))?;
        self.__trace.update_signal(&self.__trace_signal_id_reg_7, kaze::runtime::tracing::TraceValue::U32(self.__reg_reg_1))?;
        self.__trace.update_signal(&self.__trace_signal_id_reg_6, kaze::runtime::tracing::TraceValue::U32(self.__reg_reg_4))?;

        Ok(())
    }
}

