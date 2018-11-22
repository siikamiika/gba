#![feature(int_to_from_bytes)]
use std::thread::sleep;
use std::time::Duration;
use std::env::args;

mod gba;
mod memory;
mod cpu;
mod registers;
mod arm_instructions;

use self::registers::Register::*;
use self::cpu::Mode::*;
use self::arm_instructions::decode_instruction;

fn main() {
    // command line arguments
    let args: Vec<_> = args().collect();
    let bios = &args[1];
    let rom = &args[2];

    // instantiate GBA
    let mut gba = gba::Gba::new(bios, rom);

    // testing
    gba.tick();
    gba.tick();
    gba.tick();
    gba.tick();
    gba.tick();
}
















fn test() {
    // cpu testing
    let mut arm7tdmi = cpu::ARM7TDMI::new();
    // write to register R8 in user mode and check written value
    // arm7tdmi.write_register(999, R8);
    // assert_eq!(arm7tdmi.read_register(R8), 999);
    // set fast interrupt mode and ensure that the value of R8 has changed
    // arm7tdmi.set_mode(Fiq);
    // assert_ne!(arm7tdmi.read_register(R8), 999);

    // instruction testing
    println!("{:?}", decode_instruction(0b0001_0010_1111_1111_1111_1001_0000));
    println!("{:?}", decode_instruction(0xea000018)); // bios first word

    // memory testing
    let mut mem = memory::Memory::new();
    // everything gets initialized as 0
    assert_eq!(mem.read_word(0x02000000), 0);
    // test word write and read
    mem.write_word(1234, 0x02000000);
    assert_eq!(mem.read_word(0x02000000), 1234);
    // test halfword write and read
    mem.write_halfword(1234, 0x02000004);
    assert_eq!(mem.read_halfword(0x02000004), 1234);
    // test byte write and read
    mem.write_byte(0, 0x02000000);
    assert_eq!(mem.read_byte(0x02000000), 0);

    // should panic
    // println!("{}", mem.read_word(0x01ffffff));
    // mem.write_word(1234, 0x0203FFFF);

    // speed testing
    for _ in 0..100_000_000 {
        mem.read_word(0x08000000);
        // mem.read_byte(0x08000000);
        // mem.write_word(0xccddeeff, 0x02000000);
        // arm7tdmi.write_register(999, R0);
        // arm7tdmi.read_register(R0);
    }
}
