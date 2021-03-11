use kaze::*;

pub fn register<'a>(c: &'a Context<'a>) {
    let register = c.module("Register");

    let load_ = register.input("load_", 1);
    let data = register.input("data", 4);
    let reg = register.reg("reg", 4);

    reg.default_value(0x0u32);
    reg.drive_next(load_.mux(reg.value, data));
    register.output("value", reg.value);
}
