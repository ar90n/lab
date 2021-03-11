mod alu;
mod decoder;
mod pc;
mod register;
mod selector;
mod td4;
mod program;

use std::env;
use std::fs::File;
use std::path::PathBuf;

use kaze::*;

use alu::alu;
use decoder::decoder;
use pc::program_counter;
use register::register;
use selector::selector;
use td4::td4;
use program::lchika_program;
use program::timer_program;

fn create_file(name: &str) -> std::io::Result<File> {
    let mut dest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest.push("src");

    File::create(&dest.join(format!("{}.rs", name)))
}

fn main() -> std::io::Result<()> {
    // Create a context
    let c = Context::new();

    // Generate Register codes
    register(&c);
    sim::generate(
        c.modules().get("Register").unwrap(),
        sim::GenerationOptions::default(),
        create_file("register").unwrap(),
    )?;

    // Generate Decoder codes
    decoder(&c);
    sim::generate(
        c.modules().get("Decoder").unwrap(),
        sim::GenerationOptions::default(),
        create_file("decoder").unwrap(),
    )?;

    // Generate ProgramCounter codes
    program_counter(&c);
    sim::generate(
        c.modules().get("ProgramCounter").unwrap(),
        sim::GenerationOptions::default(),
        create_file("pc").unwrap(),
    )?;

    // Generate ALU codes
    alu(&c);
    sim::generate(
        c.modules().get("ALU").unwrap(),
        sim::GenerationOptions::default(),
        create_file("alu").unwrap(),
    )?;

    // Generate Selector codes
    selector(&c);
    sim::generate(
        c.modules().get("Selector").unwrap(),
        sim::GenerationOptions::default(),
        create_file("selector").unwrap(),
    )?;

    // Generate Lchika program codes
    lchika_program(&c);
    sim::generate(
        c.modules().get("LchikaProgram").unwrap(),
        sim::GenerationOptions::default(),
        //sim::GenerationOptions { tracing: true },
        create_file("lchika_program").unwrap(),
    )?;

    // Generate Lchika program codes
    timer_program(&c);
    sim::generate(
        c.modules().get("TimerProgram").unwrap(),
        sim::GenerationOptions::default(),
        create_file("timer_program").unwrap(),
    )?;

    // Generate TD4 codes
    td4(&c);
    sim::generate(
        c.modules().get("TD4").unwrap(),
        sim::GenerationOptions::default(),
        create_file("td4").unwrap(),
    )?;

    sim::generate(
        c.modules().get("TD4").unwrap(),
        sim::GenerationOptions { tracing: true },
        create_file("td4_sim").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("TD4").unwrap(),
        create_file("td4.v").unwrap(),
    )?;

    Ok(())
}
