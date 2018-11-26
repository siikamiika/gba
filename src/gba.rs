use std::io::prelude::*;
use std::fs::File;
use std::cell::RefCell;
use std::rc::Rc;

use super::cpu::ARM7TDMI;
use super::memory::Memory;
use super::arm_instructions::decode_instruction;
use super::registers::{Register, Read as Read_};

pub struct Gba {
    cpu: ARM7TDMI,
    memory: Rc<RefCell<Memory>>,
}

impl Gba {
    pub fn new(bios: &String, rom: &String) -> Self {
        let mut memory = Rc::new(RefCell::new(Memory::new()));
        let mut cpu = ARM7TDMI::new(memory.clone());

        let bios: Result<Vec<u8>, _> = File::open(bios).unwrap().bytes().collect();
        memory.borrow_mut().add_bios(bios.unwrap());

        let rom: Result<Vec<u8>, _> = File::open(rom).unwrap().bytes().collect();
        memory.borrow_mut().add_rom(rom.unwrap());

        Gba {
            cpu: cpu,
            memory: memory,
        }
    }

    pub fn tick(&mut self) {
        let pc_val = self.cpu.registers.read(Register::Pc);
        let raw_instruction = self.memory.borrow().read_word((pc_val) as usize);
        let instruction = decode_instruction(raw_instruction);
        println!("r0:   {:#010x}", self.cpu.registers.r0.read(&self.cpu.mode.borrow()));
        println!("r1:   {:#010x}", self.cpu.registers.r1.read(&self.cpu.mode.borrow()));
        println!("r2:   {:#010x}", self.cpu.registers.r2.read(&self.cpu.mode.borrow()));
        println!("r3:   {:#010x}", self.cpu.registers.r3.read(&self.cpu.mode.borrow()));
        println!("r4:   {:#010x}", self.cpu.registers.r4.read(&self.cpu.mode.borrow()));
        println!("r5:   {:#010x}", self.cpu.registers.r5.read(&self.cpu.mode.borrow()));
        println!("r6:   {:#010x}", self.cpu.registers.r6.read(&self.cpu.mode.borrow()));
        println!("r7:   {:#010x}", self.cpu.registers.r7.read(&self.cpu.mode.borrow()));
        println!("r8:   {:#010x}", self.cpu.registers.r8.read(&self.cpu.mode.borrow()));
        println!("r9:   {:#010x}", self.cpu.registers.r9.read(&self.cpu.mode.borrow()));
        println!("r10:  {:#010x}", self.cpu.registers.r10.read(&self.cpu.mode.borrow()));
        println!("r11:  {:#010x}", self.cpu.registers.r11.read(&self.cpu.mode.borrow()));
        println!("r12:  {:#010x}", self.cpu.registers.r12.read(&self.cpu.mode.borrow()));
        println!("sp:   {:#010x}", self.cpu.registers.sp.read(&self.cpu.mode.borrow()));
        println!("lr:   {:#010x}", self.cpu.registers.lr.read(&self.cpu.mode.borrow()));
        println!("pc:   {:#010x}", self.cpu.registers.pc.read(&self.cpu.mode.borrow()));
        println!("cpsr: {:#010x}", self.cpu.registers.cpsr.read(&self.cpu.mode.borrow()));
        println!("spsr: {:#010x}", self.cpu.registers.spsr.read(&self.cpu.mode.borrow()));
        println!("{:?}", instruction);
        self.cpu.execute_arm(instruction);
    }
}
