use std::cell::RefCell;
use std::rc::Rc;
use std::ops::Index;

use super::cpu::Mode;

// ------------------------------------------------------------------------- //

// registers
pub struct Registers {
    // lo-registers
    pub r0:    SimpleRegister, // general purpose, ARM and Thumb
    pub r1:    SimpleRegister, //
    pub r2:    SimpleRegister, //
    pub r3:    SimpleRegister, //
    pub r4:    SimpleRegister, //
    pub r5:    SimpleRegister, //
    pub r6:    SimpleRegister, //
    pub r7:    SimpleRegister, //
    // hi-registers
    pub r8:    BankedRegister, // general purpose, banked (FIQ only), freely usable in ARM mode only
    pub r9:    BankedRegister, //
    pub r10:   BankedRegister, //
    pub r11:   BankedRegister, //
    pub r12:   BankedRegister, //
    // special registers
    pub sp:    BankedRegister, // stack pointer (or gpr13 in ARM)
    pub lr:    BankedRegister, // link register (or gpr14 in ARM when LR is not needed)
                           // - old PC value saved here when calling subroutine with BL
                           // - when changing mode, old PC saved to new mode's bank of LR
    pub pc:    SimpleRegister, // program counter
                           // - incremented by 2 per instruction in Thumb, 4 in ARM
    pub cpsr:  SimpleRegister, // current program status register
                           // - condition codes, CPU control bits
    pub spsr:  BankedRegister, // saved program status register
                           // - CPSR saved here to the new mode's bank when mode changes

    mode:  Rc<RefCell<Mode>>,
}

impl Registers {
    pub fn new(mode: Rc<RefCell<Mode>>) -> Self {
        let banks_fiq = ActiveBanks::new(vec![1]);
        let banks_all = ActiveBanks::new(vec![1, 2, 3, 4, 5]);

        let mut cpsr = SimpleRegister::new();
        let mode_bits = mode.borrow().clone() as u32;
        cpsr.write(mode_bits , &mode.borrow());
        cpsr.write_psr_bits(
            vec![(PsrBit::I, true), (PsrBit::F, true)],
            &mode.borrow()
        );

        Registers {
            r0:     SimpleRegister::new(),
            r1:     SimpleRegister::new(),
            r2:     SimpleRegister::new(),
            r3:     SimpleRegister::new(),
            r4:     SimpleRegister::new(),
            r5:     SimpleRegister::new(),
            r6:     SimpleRegister::new(),
            r7:     SimpleRegister::new(),
            r8:     BankedRegister::new(banks_fiq.clone()),
            r9:     BankedRegister::new(banks_fiq.clone()),
            r10:    BankedRegister::new(banks_fiq.clone()),
            r11:    BankedRegister::new(banks_fiq.clone()),
            r12:    BankedRegister::new(banks_fiq.clone()),
            sp:     BankedRegister::new(banks_all.clone()),
            lr:     BankedRegister::new(banks_all.clone()),
            pc:     SimpleRegister::new(),
            cpsr:   SimpleRegister::new(),
            spsr:   BankedRegister::new(banks_all.clone()),
            mode:   mode,
        }
    }

    pub fn index(&self, register: u32) -> u32 {
        match register {
            0  => self.r0.read(&self.mode.borrow()),
            1  => self.r1.read(&self.mode.borrow()),
            2  => self.r2.read(&self.mode.borrow()),
            3  => self.r3.read(&self.mode.borrow()),
            4  => self.r4.read(&self.mode.borrow()),
            5  => self.r5.read(&self.mode.borrow()),
            6  => self.r6.read(&self.mode.borrow()),
            7  => self.r7.read(&self.mode.borrow()),
            8  => self.r8.read(&self.mode.borrow()),
            9  => self.r9.read(&self.mode.borrow()),
            10 => self.r10.read(&self.mode.borrow()),
            11 => self.r11.read(&self.mode.borrow()),
            12 => self.r12.read(&self.mode.borrow()),
            13 => self.sp.read(&self.mode.borrow()),
            14 => self.lr.read(&self.mode.borrow()),
            15 => self.pc.read(&self.mode.borrow()),
            16 => self.cpsr.read(&self.mode.borrow()),
             // => self.spsr.read(&self.mode.borrow()),
            _  => panic!(),
        }
    }

    pub fn index_write(&mut self, value: u32, register: u32) {
        match register {
            0  => self.r0.write(value,   &self.mode.borrow()),
            1  => self.r1.write(value,   &self.mode.borrow()),
            2  => self.r2.write(value,   &self.mode.borrow()),
            3  => self.r3.write(value,   &self.mode.borrow()),
            4  => self.r4.write(value,   &self.mode.borrow()),
            5  => self.r5.write(value,   &self.mode.borrow()),
            6  => self.r6.write(value,   &self.mode.borrow()),
            7  => self.r7.write(value,   &self.mode.borrow()),
            8  => self.r8.write(value,   &self.mode.borrow()),
            9  => self.r9.write(value,   &self.mode.borrow()),
            10 => self.r10.write(value,  &self.mode.borrow()),
            11 => self.r11.write(value,  &self.mode.borrow()),
            12 => self.r12.write(value,  &self.mode.borrow()),
            13 => self.sp.write(value,   &self.mode.borrow()),
            14 => self.lr.write(value,   &self.mode.borrow()),
            15 => self.pc.write(value,   &self.mode.borrow()),
            16 => self.cpsr.write(value, &self.mode.borrow()),
             // => self.spsr.write(value, &self.mode.borrow()),
            _  => panic!(),
        }
    }

    pub fn read(&self, register: Register) -> u32 {
        use self::Register::*;
        match register {
            R0   => self.r0.read(   &self.mode.borrow()),
            R1   => self.r1.read(   &self.mode.borrow()),
            R2   => self.r2.read(   &self.mode.borrow()),
            R3   => self.r3.read(   &self.mode.borrow()),
            R4   => self.r4.read(   &self.mode.borrow()),
            R5   => self.r5.read(   &self.mode.borrow()),
            R6   => self.r6.read(   &self.mode.borrow()),
            R7   => self.r7.read(   &self.mode.borrow()),
            R8   => self.r8.read(   &self.mode.borrow()),
            R9   => self.r9.read(   &self.mode.borrow()),
            R10  => self.r10.read(  &self.mode.borrow()),
            R11  => self.r11.read(  &self.mode.borrow()),
            R12  => self.r12.read(  &self.mode.borrow()),
            Sp   => self.sp.read(   &self.mode.borrow()),
            Lr   => self.lr.read(   &self.mode.borrow()),
            Pc   => self.pc.read(   &self.mode.borrow()),
            Cpsr => self.cpsr.read( &self.mode.borrow()),
            Spsr => self.spsr.read( &self.mode.borrow()),
        }
    }

    pub fn write(&mut self, value: u32, register: Register) {
        use self::Register::*;
        match register {
            R0   => self.r0.write(value,   &self.mode.borrow()),
            R1   => self.r1.write(value,   &self.mode.borrow()),
            R2   => self.r2.write(value,   &self.mode.borrow()),
            R3   => self.r3.write(value,   &self.mode.borrow()),
            R4   => self.r4.write(value,   &self.mode.borrow()),
            R5   => self.r5.write(value,   &self.mode.borrow()),
            R6   => self.r6.write(value,   &self.mode.borrow()),
            R7   => self.r7.write(value,   &self.mode.borrow()),
            R8   => self.r8.write(value,   &self.mode.borrow()),
            R9   => self.r9.write(value,   &self.mode.borrow()),
            R10  => self.r10.write(value,  &self.mode.borrow()),
            R11  => self.r11.write(value,  &self.mode.borrow()),
            R12  => self.r12.write(value,  &self.mode.borrow()),
            Sp   => self.sp.write(value,   &self.mode.borrow()),
            Lr   => self.lr.write(value,   &self.mode.borrow()),
            Pc   => self.pc.write(value,   &self.mode.borrow()),
            Cpsr => self.cpsr.write(value, &self.mode.borrow()),
            Spsr => self.spsr.write(value, &self.mode.borrow()),
        }
    }

    pub fn read_cpsr_bits(&self, bits: Vec<PsrBit>) -> Vec<bool> {
        self.cpsr.read_psr_bits(bits, &self.mode.borrow())
    }

    pub fn write_cpsr_bits(&mut self, bits: Vec<(PsrBit, bool)>) {
        self.cpsr.write_psr_bits(bits, &self.mode.borrow());
    }
}

// register enum
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    Sp,
    Lr,
    Pc,
    Cpsr,
    Spsr,
}

// PSR bits
pub enum PsrBit {
    // logical/arithmetic result info
    // often optional in ARM
    N  = 31, // 0 = not Negative       , 1 = Negative
    Z  = 30, // 0 = not Zero           , 1 = Zero
    C  = 29, // 0 = borrow / no Carry  , 1 = Carry / no borrow
    V  = 28, // 0 = no oVerflow        , 1 = oVerflow
    // Used by QADD, QSUB, QDADD, QDSUB, SMLAxy, and SMLAWy only
    // can be reset by MSR/MRS only
    Q  = 27, // 0 = no sticky overflow , 1 = sticky overflow
    // ------------------
    // 26-8 reserved
    // ------------------
    // control bits
    I  = 7,  // 0 = IRQ enable         , 1 = IRQ disable
    F  = 6,  // 0 = FIQ enable         , 1 = FIQ disable
    T  = 5,  // 0 = ARM                , 1 = THUMB; only set by BX
    M4 = 4,  // mode bits (current operating mode):
    M3 = 3,  // Binary Hex Dec  Expl.
    M2 = 2,  // 0xx00b 00h 0  - Old User       ;\26bit Backward Compatibility modes
    M1 = 1,  // 0xx01b 01h 1  - Old FIQ        ; (supported only on ARMv3, except ARMv3G,
    M0 = 0,  // 0xx10b 02h 2  - Old IRQ        ; and on some non-T variants of ARMv4)
             // 0xx11b 03h 3  - Old Supervisor ;/
             // 10000b 10h 16 - User (non-privileged)
             // 10001b 11h 17 - FIQ
             // 10010b 12h 18 - IRQ
             // 10011b 13h 19 - Supervisor (SWI)
             // 10111b 17h 23 - Abort
             // 11011b 1Bh 27 - Undefined
             // 11111b 1Fh 31 - System (privileged 'User' mode) (ARMv4 and up)
}

// ------------------------------------------------------------------------- //

// register traits
trait Read {
    fn read(&self, bank: &Mode) -> u32;

    fn read_bits(&self, bits: Vec<u8>, bank: &Mode) -> Vec<bool> {
        let mut result_bits = vec![];
        let val = self.read(bank);
        for bit in bits {
            result_bits.push((val >> bit) & 0b1 != 0);
        }

        result_bits
    }

    fn read_psr_bits(&self, bits: Vec<PsrBit>, bank: &Mode) -> Vec<bool> {
        let bits: Vec<u8> = bits.into_iter().map(|b| b as u8).collect();
        self.read_bits(bits, bank)
    }
}

trait Write: Read {
    fn write(&mut self, val: u32, bank: &Mode);

    fn write_bits(&mut self, bits: Vec<(u8, bool)>, bank: &Mode) {
        let mut val = self.read(bank);
        for (bit, bit_val) in bits {
            println!("{:?} {}", bit, bit_val);
            if bit_val {
                val |= 0b1 << bit
            } else {
                val &= !(0b1 << bit)
            }
        }
        self.write(val, bank);
    }

    fn write_psr_bits(&mut self, bits: Vec<(PsrBit, bool)>, bank: &Mode) {
        let bits: Vec<(u8, bool)> = bits.into_iter().map(|e| (e.0 as u8, e.1)).collect();
        self.write_bits(bits, bank);
    }
}

// ------------------------------------------------------------------------- //

// register without banks
pub struct SimpleRegister {
    pub val: u32,
}

impl SimpleRegister {
    pub fn new() -> Self {
        SimpleRegister {
            val: 0,
        }
    }
}

impl Read for SimpleRegister {
    fn read(&self, _: &Mode) -> u32 {
        self.val
    }
}

impl Write for SimpleRegister {
    fn write(&mut self, val: u32, _: &Mode) {
        self.val = val;
    }

}

// ------------------------------------------------------------------------- //

// register with banks
#[derive(Copy)]
pub struct ActiveBanks {
    banks: u8,
}

impl Clone for ActiveBanks {
    fn clone(&self) -> ActiveBanks { *self }
}

impl ActiveBanks {
    pub fn new(banks: Vec<u8>) -> Self {
        let mut selected_banks = 0;
        for bank in banks {
            selected_banks += 1 << bank
        }

        ActiveBanks {
            banks: selected_banks,
        }
    }

    pub fn is_active(&self, bank: &Mode) -> bool {
        match bank {
            Mode::Usr => (self.banks & 0b000001) != 0,
            Mode::Fiq => (self.banks & 0b000010) != 0,
            Mode::Svc => (self.banks & 0b000100) != 0,
            Mode::Abt => (self.banks & 0b001000) != 0,
            Mode::Irq => (self.banks & 0b010000) != 0,
            Mode::Und => (self.banks & 0b100000) != 0,
            Mode::Sys => (self.banks & 0b000001) != 0,
        }
    }
}

pub struct BankedRegister {
    val_usr: u32,
    val_fiq: u32,
    val_svc: u32,
    val_abt: u32,
    val_irq: u32,
    val_und: u32,
    banks:   ActiveBanks,
}

impl BankedRegister {
    pub fn new(banks: ActiveBanks) -> Self {
        BankedRegister {
            val_usr: 0,
            val_fiq: 0,
            val_svc: 0,
            val_abt: 0,
            val_irq: 0,
            val_und: 0,
            banks: banks,
        }
    }
}

impl Read for BankedRegister {
    fn read(&self, bank: &Mode) -> u32 {
        match bank {
            Mode::Usr => self.val_usr,
            Mode::Fiq => if self.banks.is_active(bank) { self.val_fiq } else { self.val_usr },
            Mode::Svc => if self.banks.is_active(bank) { self.val_svc } else { self.val_usr },
            Mode::Abt => if self.banks.is_active(bank) { self.val_abt } else { self.val_usr },
            Mode::Irq => if self.banks.is_active(bank) { self.val_irq } else { self.val_usr },
            Mode::Und => if self.banks.is_active(bank) { self.val_und } else { self.val_usr },
            Mode::Sys => self.val_usr,
        }
    }
}

impl Write for BankedRegister {
    fn write(&mut self, val: u32, bank: &Mode) {
        match bank {
            Mode::Usr => self.val_usr = val,
            Mode::Fiq => if self.banks.is_active(bank) { self.val_fiq = val } else { self.val_usr = val },
            Mode::Svc => if self.banks.is_active(bank) { self.val_svc = val } else { self.val_usr = val },
            Mode::Abt => if self.banks.is_active(bank) { self.val_abt = val } else { self.val_usr = val },
            Mode::Irq => if self.banks.is_active(bank) { self.val_irq = val } else { self.val_usr = val },
            Mode::Und => if self.banks.is_active(bank) { self.val_und = val } else { self.val_usr = val },
            Mode::Sys => self.val_usr = val,
        }
    }
}

// ------------------------------------------------------------------------- //
