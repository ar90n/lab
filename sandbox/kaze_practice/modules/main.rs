use std::env;
use std::fs::File;
use std::path::PathBuf;

use kaze::*;

mod register;
use register::register;

mod decoder;
use decoder::decoder;

mod pc;
use pc::program_counter;

mod alu;
use alu::alu;

fn create_file(name: &str) -> std::io::Result<File> {
    let mut dest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest.push("src");

    File::create(&dest.join(format!("{}.rs", name)))
}

fn main() -> std::io::Result<()> {
    // Create a context, which will contain our module(s)
    let c = Context::new();

    // Generate Rust simulator code

    let register = register(&c);
    sim::generate(
        register,
        sim::GenerationOptions::default(),
        create_file("register").unwrap(),
    )?;

    let decoder = decoder(&c);
    sim::generate(
        decoder,
        sim::GenerationOptions::default(),
        create_file("decoder").unwrap(),
    )?;

    let pc = program_counter(&c);
    sim::generate(
        pc,
        sim::GenerationOptions::default(),
        create_file("pc").unwrap(),
    )?;

    let alu = alu(&c);
    sim::generate(
        alu,
        sim::GenerationOptions::default(),
        create_file("alu").unwrap(),
    )?;
    // Generate Verilog code
    //verilog::generate(inverter, std::io::stdout())?;

    Ok(())
}
