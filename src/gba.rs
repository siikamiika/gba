use std::io::prelude::*;
use std::fs::File;

use super::cpu::ARM7TDMI;
use super::memory::Memory;
use super::arm_instructions::decode_instruction;
use super::registers::{Register, Read as Read_};

pub struct Gba {
    cpu: ARM7TDMI,
    memory: Memory,
}

impl Gba {
    pub fn new(bios: &String, rom: &String) -> Self {
        let mut cpu = ARM7TDMI::new();
        let mut memory = Memory::new();

        let bios: Result<Vec<u8>, _> = File::open(bios).unwrap().bytes().collect();
        memory.add_bios(bios.unwrap());

        let rom: Result<Vec<u8>, _> = File::open(rom).unwrap().bytes().collect();
        memory.add_rom(rom.unwrap());

        Gba {
            cpu: cpu,
            memory: memory,
        }
    }

    pub fn tick(&mut self) {
        let pc_val = self.cpu.registers.read(Register::Pc);
        let raw_instruction = self.memory.read_word(pc_val as usize);
        let instruction = decode_instruction(raw_instruction);
        println!("sp:   {:#010x}", self.cpu.registers.sp.read(&self.cpu.mode.borrow()));
        println!("lr:   {:#034b}", self.cpu.registers.lr.read(&self.cpu.mode.borrow()));
        println!("pc:   {:#010x}", self.cpu.registers.pc.read(&self.cpu.mode.borrow()));
        println!("cpsr: {:#034b}", self.cpu.registers.cpsr.read(&self.cpu.mode.borrow()));
        println!("{:?}", instruction);
        self.cpu.execute_arm(instruction);
    }
}
