use std::cell::RefCell;
use std::rc::Rc;

use super::registers::{Registers, Register};
use super::memory::Memory;

pub struct ARM7TDMI {
    state: State,
    pub registers: Registers,
    pub mode: Rc<RefCell<Mode>>,
    pub memory: Rc<RefCell<Memory>>
}

impl ARM7TDMI {
    pub fn new(memory: Rc<RefCell<Memory>>) -> Self {
        let mode = Rc::new(RefCell::new(Mode::Svc));
        ARM7TDMI {
            state: State::Arm,
            registers: Registers::new(mode.clone()),
            mode: mode,
            memory: memory,
        }
    }

    fn set_mode(&mut self, mode: Mode) {
        let mode_bits = mode.clone() as u32;
        let mut cpsr = self.registers.read(Register::Cpsr);
        cpsr = cpsr - (cpsr & (1 << 5) - 1) + mode_bits;
        self.registers.write(cpsr, Register::Cpsr);

        *self.mode.borrow_mut() = mode;
    }
}

#[derive(Clone, Debug)]
pub enum Mode {
    Usr = 0b10000,  // user
    Fiq = 0b10001,  // fast interrupt
    Irq = 0b10010,  // interrupt
    Svc = 0b10011,  // supervisor
    Abt = 0b10111,  // abort
    Und = 0b11011,  // undefined
    Sys = 0b11111,  // system (same as user on the GBA)
    Wtf,
}

impl From<u32> for Mode {
    fn from(num: u32) -> Self {
        use self::Mode::*;
        match num {
            0b10000 => Usr,
            0b10001 => Fiq,
            0b10010 => Irq,
            0b10011 => Svc,
            0b10111 => Abt,
            0b11011 => Und,
            0b11111 => Sys,
            _       => Wtf,
        }
    }
}

// changed with BX instruction, or automatically to ARM when executing exception
// (switch back manually)
pub enum State {
    Arm,    // 32 bit opcodes
    Thumb,  // 16 bit opcodes
}
