use std::cell::RefCell;
use std::rc::Rc;

use super::registers::{Registers, Register};

pub struct ARM7TDMI {
    state: State,
    pub registers: Registers,
    mode: Rc<RefCell<Mode>>,
}

impl ARM7TDMI {
    pub fn new() -> Self {
        let mode = Rc::new(RefCell::new(Mode::Svc));
        ARM7TDMI {
            state: State::Arm,
            registers: Registers::new(mode.clone()),
            mode: mode,
        }
    }

    fn set_mode(&mut self, mode: Mode) {
        let mode_bits = mode.clone() as u32;
        self.registers.write(mode_bits , Register::Cpsr);

        *self.mode.borrow_mut() = mode;
    }
}

#[derive(Clone)]
pub enum Mode {
    Usr = 0b10000,  // user
    Fiq = 0b10001,  // fast interrupt
    Irq = 0b10010,  // interrupt
    Svc = 0b10011,  // supervisor
    Abt = 0b10111,  // abort
    Und = 0b11011,  // undefined
    Sys = 0b11111,  // system (same as user on the GBA)
}

// changed with BX instruction, or automatically to ARM when executing exception
// (switch back manually)
pub enum State {
    Arm,    // 32 bit opcodes
    Thumb,  // 16 bit opcodes
}
