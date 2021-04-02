mod alu;
mod decoder;
mod pc;
mod register;
mod selector;
mod td4;
mod memory;

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
use memory::memory;

fn create_file(name: &str, suffix: &str) -> std::io::Result<File> {
    let mut dest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest.push("src");

    File::create(&dest.join(format!("{}.{}", name, suffix)))
}

fn main() -> std::io::Result<()> {
    // Create a context
    let c = Context::new();

    // Generate Register codes
    register(&c);
    sim::generate(
        c.modules().get("Register").unwrap(),
        sim::GenerationOptions::default(),
        create_file("register", "rs").unwrap(),
    )?;
    verilog::generate(
        c.modules().get("Register").unwrap(),
        create_file("register", "v").unwrap(),
    )?;


    // Generate Decoder codes
    decoder(&c);
    sim::generate(
        c.modules().get("Decoder").unwrap(),
        sim::GenerationOptions::default(),
        create_file("decoder", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("Decoder").unwrap(),
        create_file("decoder", "v").unwrap(),
    )?;


    // Generate ProgramCounter codes
    program_counter(&c);
    sim::generate(
        c.modules().get("ProgramCounter").unwrap(),
        sim::GenerationOptions::default(),
        create_file("pc", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("ProgramCounter").unwrap(),
        create_file("pc", "v").unwrap(),
    )?;

    // Generate ALU codes
    alu(&c);
    sim::generate(
        c.modules().get("ALU").unwrap(),
        sim::GenerationOptions::default(),
        create_file("alu", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("ALU").unwrap(),
        create_file("alu", "v").unwrap(),
    )?;

    // Generate Selector codes
    selector(&c);
    sim::generate(
        c.modules().get("Selector").unwrap(),
        sim::GenerationOptions::default(),
        create_file("selector", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("Selector").unwrap(),
        create_file("selector", "v").unwrap(),
    )?;

    // Generate Memory codes
    memory(&c);
    sim::generate(
        c.modules().get("Memory").unwrap(),
        sim::GenerationOptions::default(),
        create_file("memory", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("Memory").unwrap(),
        create_file("memory", "v").unwrap(),
    )?;

    // Generate TD4 codes
    td4(&c);
    sim::generate(
        c.modules().get("TD4").unwrap(),
        sim::GenerationOptions::default(),
        create_file("td4", "rs").unwrap(),
    )?;

    sim::generate(
        c.modules().get("TD4").unwrap(),
        sim::GenerationOptions { tracing: true },
        create_file("td4_sim", "rs").unwrap(),
    )?;

    verilog::generate(
        c.modules().get("TD4").unwrap(),
        create_file("td4", "v").unwrap(),
    )?;

    Ok(())
}
