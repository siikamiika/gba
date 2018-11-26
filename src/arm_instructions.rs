use std::ops::{Add, Sub};

use super::cpu::ARM7TDMI;
use super::registers::{Register, PsrBit, Read};

impl ARM7TDMI {
    // TODO: return instruction execution time
    pub fn execute_arm(&mut self, instruction: Instruction) {
        use self::Instruction::*;
        match instruction {
            And{cond, i, s, rn, rd, op2} => self.exec_arm_and(cond, i, s, rn, rd, op2),
            Eor{cond, i, s, rn, rd, op2} => self.exec_arm_eor(cond, i, s, rn, rd, op2),
            Sub{cond, i, s, rn, rd, op2} => self.exec_arm_sub(cond, i, s, rn, rd, op2),
            Rsb{cond, i, s, rn, rd, op2} => self.exec_arm_rsb(cond, i, s, rn, rd, op2),
            Add{cond, i, s, rn, rd, op2} => self.exec_arm_add(cond, i, s, rn, rd, op2),
            Adc{cond, i, s, rn, rd, op2} => self.exec_arm_adc(cond, i, s, rn, rd, op2),
            Sbc{cond, i, s, rn, rd, op2} => self.exec_arm_sbc(cond, i, s, rn, rd, op2),
            Rsc{cond, i, s, rn, rd, op2} => self.exec_arm_rsc(cond, i, s, rn, rd, op2),
            Tst{cond, i, s, rn, rd, op2} => self.exec_arm_tst(cond, i, s, rn, rd, op2),
            Teq{cond, i, s, rn, rd, op2} => self.exec_arm_teq(cond, i, s, rn, rd, op2),
            Cmp{cond, i, s, rn, rd, op2} => self.exec_arm_cmp(cond, i, s, rn, rd, op2),
            Cmn{cond, i, s, rn, rd, op2} => self.exec_arm_cmn(cond, i, s, rn, rd, op2),
            Orr{cond, i, s, rn, rd, op2} => self.exec_arm_orr(cond, i, s, rn, rd, op2),
            Mov{cond, i, s, rn, rd, op2} => self.exec_arm_mov(cond, i, s, rn, rd, op2),
            Bic{cond, i, s, rn, rd, op2} => self.exec_arm_bic(cond, i, s, rn, rd, op2),
            Mvn{cond, i, s, rn, rd, op2} => self.exec_arm_mvn(cond, i, s, rn, rd, op2),
            B{cond, l, offset} => self.exec_arm_b(cond, l, offset),
            Bx{cond, rn} => self.exec_arm_bx(cond, rn),
            Cdp{cond, c_opc, crn, crd, cn, cinf, crm} => self.exec_arm_cdp(cond, c_opc, crn, crd, cn, cinf, crm),
            LdcStc{cond, p, u, n, w, l, rn, crd, cn, offset} => self.exec_arm_ldcstc(cond, p, u, n, w, l, rn, crd, cn, offset),
            LdmStm{cond, p, u, s, w, l, rn, regs} => self.exec_arm_ldmstm(cond, p, u, s, w, l, rn, regs),
            LdrStr{cond, i, p, u, b, w, l, rn, rd, offset} => self.exec_arm_ldrstr(cond, i, p, u, b, w, l, rn, rd, offset),
            LdrStrHalf{cond, p, u, i, w, l, rn, rd, s, h, offset} => self.exec_arm_ldrstrhalf(cond, p, u, i, w, l, rn, rd, s, h, offset),
            Mrs{cond, ps, rd} => self.exec_arm_mrs(cond, ps, rd),
            Msr{cond, i, pd, f, op} => self.exec_arm_msr(cond, i, pd, f, op),
            Mul{cond, a, s, rd, rn, rs, rm} => self.exec_arm_mul(cond, a, s, rd, rn, rs, rm),
            Mull{cond, u, a, s, rd_hi, rd_lo, rn, rm} => self.exec_arm_mull(cond, u, a, s, rd_hi, rd_lo, rn, rm),
            Swi{cond, comment} => self.exec_arm_swi(cond, comment),
            Swp{cond, b, rn, rd, rm} => self.exec_arm_swp(cond, b, rn, rd, rm),
            Und{cond} => self.exec_arm_und(cond),
            _ => panic!("unimplemented! {:?}", instruction),
        }

        let old_pc = self.registers.read(Register::Pc);
        self.registers.write(old_pc + 4, Register::Pc);
    }

    // ------------------------------------------------------------------------- //

    fn arm_condition_true(&self, cond: Condition) -> bool {
        use self::PsrBit::*;
        use self::Condition::*;
        match cond {
            Eq => self.registers.read_cpsr_bits(vec![Z])[0],
            Ne => !self.registers.read_cpsr_bits(vec![Z])[0],
            Cs => self.registers.read_cpsr_bits(vec![C])[0],
            Cc => !self.registers.read_cpsr_bits(vec![Z])[0],
            Mi => self.registers.read_cpsr_bits(vec![N])[0],
            Pl => !self.registers.read_cpsr_bits(vec![N])[0],
            Vs => self.registers.read_cpsr_bits(vec![V])[0],
            Vc => !self.registers.read_cpsr_bits(vec![V])[0],
            Hi => self.registers.read_cpsr_bits(vec![C, Z]) == vec![true, false],
            Ls => self.registers.read_cpsr_bits(vec![C, Z]) == vec![false, true],
            Ge => {
                let bits = self.registers.read_cpsr_bits(vec![N,  V]);
                bits[0] == bits[1]
            },
            Lt => {
                let bits = self.registers.read_cpsr_bits(vec![N, V]);
                bits[0] != bits[1]
            },
            Gt => {
                let bits = self.registers.read_cpsr_bits(vec![Z, N, V]);
                !bits[0] && (bits[1] == bits[2])
            },
            Le => {
                let bits = self.registers.read_cpsr_bits(vec![Z, N, V]);
                bits[0] && (bits[1] != bits[2])
            },
            Al => true,
        }
    }

    fn arm_shift_op2_reg(&mut self, op2: u32, s: bool) -> u32 {
        use self::PsrBit::*;
        let rm_val = self.registers.index(op2 & 0xf);
        let shift_type = (op2 >> 5) & 0b11;
        let shift;
        if op2 & 0b1_0000 == 0b1_0000 {
            shift = self.registers.index((op2 >> 8) & 0xff);
            // special case
            if shift == 0 {
                return rm_val
            }
        } else {
            shift = op2 >> 7;
        }
        let mut c = self.registers.read_cpsr_bits(vec![C])[0];
        let result = match shift_type {
            // LSL
            0b00 => {
                let mut out = 0;
                if shift == 32 {
                    c = rm_val & 0b1 == 1;
                    out = 0;
                } else if shift < 32 {
                    c = rm_val >> (31 - shift) == 1;
                    out = rm_val << shift;
                } else {
                    c = false;
                }
                out
            },
            // LSR
            0b01 => {
                let mut out = 0;
                if shift == 0 || shift == 32 {
                    c = rm_val >> 31 == 1;
                } else if shift > 32 {
                    c = false;
                } else {
                    c = (rm_val >> shift - 1) & 0b1 == 1;
                    out = rm_val >> shift;
                }
                out
            },
            // ASR
            0b10 => {
                let mut shift = shift;
                if shift == 0 || shift >= 32 {
                    c = rm_val >> 31 == 1;
                    shift = 31;
                } else {
                    c = (rm_val >> shift - 1) & 0b1 == 1;
                }
                ((rm_val as i32) >> shift) as u32
            },
            // ROR
            0b11 => {
                let out: u32;
                // TODO: wrong timing?
                let shift = shift % 32;
                if shift == 0 {
                    c = rm_val >> 31 == 1;
                    out = ((c as u32) << 31) + (rm_val >> 1);
                } else {
                    c = (rm_val >> shift - 1) & 0b1 == 1;
                    out = rm_val.rotate_right(shift)
                }
                out
            },
            _    => panic!(),
        };

        if s {
            let n = result > 0x7fffffff;
            let z = result == 0;
            let v = n && rm_val <= 0x7fffffff;
            self.registers.write_cpsr_bits(vec![(V, v), (C, c), (Z, z), (N, n)]);
        }

        result
    }

    fn arm_shift_op2_imm(&self, op2: u32) -> u32 {
        let imm_val = op2 & 0xff;
        let rot = op2 >> 8;
        imm_val.rotate_right(rot * 2)
    }

    fn exec_arm_data_proc(&mut self, f: &Fn(u32, u32) -> u32, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, mut op2: u32) {
        use self::PsrBit::*;
        if !self.arm_condition_true(cond) {return};

        if i  {
            op2 = self.arm_shift_op2_imm(op2);
        } else {
            op2 = self.arm_shift_op2_reg(op2, s);
        }

        let result = f(self.registers.index(rn), op2);
        if s {
            let n = result > 0x7fffffff;
            let z = result == 0;
            self.registers.write_cpsr_bits(vec![(Z, z), (N, n)]);
        }
        if rd == 15 {
            self.registers.write(self.registers.read(Register::Spsr), Register::Cpsr);
        }
        self.registers.index_write(result, rd);
    }

    // ------------------------------------------------------------------------- //

    fn exec_arm_and(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn & op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_eor(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn ^ op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_sub(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn - op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_rsb(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {op2 - rn}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_add(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn + op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_adc(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let c = self.registers.read_cpsr_bits(vec![self::PsrBit::C])[0] as u32;
        self.exec_arm_data_proc(&|rn, op2| {rn + op2 + c}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_sbc(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let c = self.registers.read_cpsr_bits(vec![self::PsrBit::C])[0] as u32;
        self.exec_arm_data_proc(&|rn, op2| {rn - op2 - 1 + c}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_rsc(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let c = self.registers.read_cpsr_bits(vec![self::PsrBit::C])[0] as u32;
        self.exec_arm_data_proc(&|rn, op2| {op2 - rn - 1 + c}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_tst(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let rd_val = self.registers.index(rd);
        self.exec_arm_data_proc(&|rn, op2| {rn & op2}, cond, i, s, rn, rd, op2);
        self.registers.index_write(rd_val, rd);
    }

    fn exec_arm_teq(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let rd_val = self.registers.index(rd);
        self.exec_arm_data_proc(&|rn, op2| {rn ^ op2}, cond, i, s, rn, rd, op2);
        self.registers.index_write(rd_val, rd);
    }

    fn exec_arm_cmp(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let rd_val = self.registers.index(rd);
        self.exec_arm_data_proc(&|rn, op2| {rn - op2}, cond, i, s, rn, rd, op2);
        self.registers.index_write(rd_val, rd);
    }


    fn exec_arm_cmn(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        let rd_val = self.registers.index(rd);
        self.exec_arm_data_proc(&|rn, op2| {rn + op2}, cond, i, s, rn, rd, op2);
        self.registers.index_write(rd_val, rd);
    }

    fn exec_arm_orr(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn | op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_mov(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|_, op2| {op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_bic(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|rn, op2| {rn & !op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_mvn(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        self.exec_arm_data_proc(&|_, op2| {0xffffffff ^ op2}, cond, i, s, rn, rd, op2);
    }

    fn exec_arm_b(&mut self, cond: Condition, l: bool, offset: u32) {
        if !self.arm_condition_true(cond) {return};

        let sign = (offset << 8) & 0x8000_0000;
        let offset = sign + ((offset & 0x7f_ffff) << 2);

        let old_pc = self.registers.read(Register::Pc) + 4;
        if l {
            self.registers.write(old_pc & !0b11, Register::Lr);
        }
        self.registers.write(old_pc + offset, Register::Pc);
    }

    // TODO: Thumb
    fn exec_arm_bx(&mut self, cond: Condition, rn: u32) {
        if !self.arm_condition_true(cond) {return};

        let rn_val = self.registers.index(rn);

        if rn_val & 0b1 == 1 {
            self.registers.write((rn_val & !0b1) + 2, Register::Pc);
            panic!("Thumb unimplemented!");
        } else {
            self.registers.write((rn_val & !0b11) + 4, Register::Pc);
        }
    }

    // TODO: unimplemented
    fn exec_arm_cdp(&mut self, cond: Condition, c_opc: u32, crn: u32, crd: u32, cn: u32, cinf: u32, crm: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_ldcstc(&mut self, cond: Condition, p: bool, u: bool, n: bool, w: bool, l: bool, rn: u32, crd: u32, cn: u32, offset: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_ldmstm(&mut self, cond: Condition, p: bool, u: bool, s: bool, w: bool, l: bool, rn: u32, regs: u32) {
        if !self.arm_condition_true(cond) {return};
        // Whenever R15 is stored to memory the stored value is the address of the STM instruction plus 12.
    }

    fn exec_arm_ldrstr(&mut self, cond: Condition, i: bool, p: bool, u: bool, b: bool, w: bool, l: bool, rn: u32, rd: u32, mut offset: u32) {
        if !self.arm_condition_true(cond) {return};

        // offset is shifted register
        if i {
            // if offset & 0b1_0000 == 0 {
            offset = self.arm_shift_op2_reg(offset, false);
            // } else {
            //     offset = self.registers.index(offset & 0xf);
            // }
        }

        // up/down
        let oper: &Fn(u32, u32) -> u32;
        if u {
            oper = &Add::add;
        } else {
            oper = &Sub::sub;
        }

        let address;

        // pre-index
        if p {
            address = oper(self.registers.index(rn), offset) & !0b11;
            if w {
                self.registers.index_write(address, rn);
            }
        // post-index
        } else {
            address = self.registers.index(rn) & !0b11;
            self.registers.index_write(oper(address, offset), rn);
        }

        // byte
        if b {
            // load
            if l {
                let val = self.memory.borrow().read_byte(address as usize);
                self.registers.index_write(val as u32, rd);
            // store
            } else {
                let val = self.registers.index(rd) as u8;
                self.memory.borrow_mut().write_byte(val, address as usize);
            }
        // word
        } else {
            // load
            if l {
                let val = self.memory.borrow().read_word(address as usize);
                self.registers.index_write(val as u32, rd);
            // store
            } else {
                let val = self.registers.index(rd);
                self.memory.borrow_mut().write_word(val, address as usize);
            }
        }
    }

    // TODO: unimplemented
    fn exec_arm_ldrstrhalf(&mut self, cond: Condition, p: bool, u: bool, i: bool, w: bool, l: bool, rn: u32, rd: u32, s: bool, h: bool, offset: u32) {
    }

    fn exec_arm_mrs(&mut self, cond: Condition, ps: bool, rd: u32) {
        if !self.arm_condition_true(cond) {return};

        let psr_val: u32;
        if ps {
            psr_val = self.registers.read(Register::Spsr);
        } else {
            psr_val = self.registers.read(Register::Cpsr);
        }

        self.registers.index_write(psr_val, rd);
    }

    fn exec_arm_msr(&mut self, cond: Condition, i: bool, pd: bool, f: bool, op: u32) {
        if !self.arm_condition_true(cond) {return};

        let rm_val = self.registers.index(op & 0xf);

        let psr_val = if i {
            self.arm_shift_op2_imm(op)
        } else {
            rm_val
        };

        if pd {
            let old = self.registers.read(Register::Spsr);
            if f {
                self.registers.write(old & 0x0fffffff | psr_val, Register::Spsr);
            } else {
                self.registers.write(psr_val, Register::Spsr);
            }
        } else {
            let old = self.registers.read(Register::Cpsr);
            if f {
                self.registers.write(old & 0x0fffffff | psr_val, Register::Cpsr);
            } else {
                self.registers.write(psr_val, Register::Cpsr);
            }
        }
    }

    // TODO: unimplemented
    fn exec_arm_mul(&mut self, cond: Condition, a: bool, s: bool, rd: u32, rn: u32, rs: u32, rm: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_mull(&mut self, cond: Condition, u: bool, a: bool, s: bool, rd_hi: u32, rd_lo: u32, rn: u32, rm: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_swi(&mut self, cond: Condition, comment: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_swp(&mut self, cond: Condition, b: bool, rn: u32, rd: u32, rm: u32) {
    }

    // TODO: unimplemented
    fn exec_arm_und(&mut self, cond: Condition) {
    }
}


// ========================================================================= //

// condition field; bits 31 to 28 inclusive
#[derive(Debug)]
pub enum Condition {
    Eq, // Z, equal
    Ne, // !Z, not equal
    Cs, // C, unsigned higher or same
    Cc, // !C unsigned lower
    Mi, // N, negative
    Pl, // !N, positive or zero
    Vs, // V, overflow
    Vc, // !V, no overflow
    Hi, // C and !Z, unsigned higher
    Ls, // !C or Z, unsigned lower or same
    Ge, // N == V, greater or equal
    Lt, // N != V, less than
    Gt, // !Z and (N == V) greater than
    Le, // Z or (N != V) less than or equal
    Al, // always
}

fn decode_condition(instr: u32) -> Condition {
    use self::Condition::*;

    let condition = instr >> 28;
    match condition {
        0b0000 => Eq,
        0b0001 => Ne,
        0b0010 => Cs,
        0b0011 => Cc,
        0b0100 => Mi,
        0b0101 => Pl,
        0b0110 => Vs,
        0b0111 => Vc,
        0b1000 => Hi,
        0b1001 => Ls,
        0b1010 => Ge,
        0b1011 => Lt,
        0b1100 => Gt,
        0b1101 => Le,
        0b1110 => Al,
        _      => panic!(),
    }
}

pub fn decode_instruction(instr: u32) -> Instruction {
    use self::Instruction::*;

    let mut bits = vec![];
    for i in 0..=31 {
        bits.push(((instr >> i) & 0b1) == 1);
    }

    let mut nibbles = vec![];
    for i in (0..=31).step_by(4) {
        nibbles.push((instr >> i) & 0xf);
    }

    let condition = decode_condition(instr);

    // ---------------- 27 = 0, 26 = 0 ----------------------
    return if instr & 0b1111_1111_1111_1111_1111_1111_0000 == 0b0001_0010_1111_1111_1111_0001_0000 {
        // Branch and Exchange
        Bx {cond: condition, rn: nibbles[0]}
    } else if instr & 0b1111_1100_0000_0000_0000_1111_0000 == 0b0000_0000_0000_0000_0000_1001_0000 {
        // Multiply
        Mul {cond: condition, a: bits[21], s: bits[20], rd: nibbles[4], rn: nibbles[3], rs: nibbles[2], rm: nibbles[0]}
    } else if instr & 0b1111_1000_0000_0000_0000_1111_0000 == 0b0000_1000_0000_0000_0000_1001_0000 {
        // Multiply Long
        Mull {cond: condition, u: bits[22], a: bits[21], s: bits[20], rd_hi: nibbles[4], rd_lo: nibbles[3], rn: nibbles[2], rm: nibbles[0]}
    } else if instr & 0b1111_1011_0000_0000_1111_1111_0000 == 0b0001_0000_0000_0000_0000_1001_0000 {
        // Single Data Swap
        Swp {cond: condition, b: bits[22], rn: nibbles[4], rd: nibbles[3], rm: nibbles[0]}
    } else if instr & 0b1110_0100_0000_0000_1111_1001_0000 == 0b0000_0000_0000_0000_0000_1001_0000 {
        // Halfword Data Transfer: register offset
        LdrStrHalf {cond: condition, p: bits[24], u: bits[23], i: bits[22], w: bits[21], l: bits[20], rn: nibbles[4], rd: nibbles[3], s: bits[6], h: bits[5], offset: nibbles[0]}
    } else if instr & 0b1110_0100_0000_0000_0000_1001_0000 == 0b0000_0100_0000_0000_0000_1001_0000 {
        // Halfword Data Transfer: immediate offset
        LdrStrHalf {cond: condition, p: bits[24], u: bits[23], i: bits[22], w: bits[21], l: bits[20], rn: nibbles[4], rd: nibbles[3], s: bits[6], h: bits[5], offset: (nibbles[2] << 4) + nibbles[0]}
    } else if instr & 0b1111_1011_1111_0000_1111_1111_1111 == 0b0001_0000_1111_0000_0000_0000_0000 {
        // MRS (transfer PSR contents to a register)
        Mrs {cond: condition, ps: bits[22], rd: nibbles[3]}
    } else if instr & 0b1111_1011_1111_1111_1111_1111_0000 == 0b0001_0010_1001_1111_0000_0000_0000 {
        // MSR (transfer register contents to PSR)
        Msr {cond: condition, i: false, f: false, pd: bits[22], op: instr & 0xfff}
    } else if instr & 0b1101_1011_1111_1111_0000_0000_0000 == 0b0001_0010_1000_1111_0000_0000_0000 {
        // MSR (transfer register contents or immdiate value to PSR flag bits only)
        Msr {cond: condition, i: bits[25], f: !bits[16], pd: bits[22], op: instr & 0xfff}
    } else if instr & 0b1100_0000_0000_0000_0000_0000_0000 == 0b0000_0000_0000_0000_0000_0000_0000 {
        // Data Processing / PSR Transfer
        let op2 = instr & 0xfff;
        match (instr >> 21) & 0b1111 {
            0b0000 => And {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0001 => Eor {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0010 => Sub {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0011 => Rsb {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0100 => Add {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0101 => Adc {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0110 => Sbc {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b0111 => Rsc {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1000 => Tst {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1001 => Teq {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1010 => Cmp {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1011 => Cmn {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1100 => Orr {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1101 => Mov {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1110 => Bic {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            0b1111 => Mvn {cond: condition, i: bits[25], s: bits[20], rn: nibbles[4], rd: nibbles[3], op2: op2},
            _      => panic!(),
        }
    }
    // ---------------- 27 = 0, 26 = 1 ----------------------
    else if   instr & 0b1100_0000_0000_0000_0000_0000_0000 == 0b0100_0000_0000_0000_0000_0000_0000 {
        // Single Data Transfer
        LdrStr {cond: condition, i: bits[25], p: bits[24], u: bits[23], b: bits[22], w: bits[21], l: bits[20], rn: nibbles[4], rd: nibbles[3], offset: instr & 0xfff}
    } else if instr & 0b1110_0000_0000_0000_0000_0001_0000 == 0b0110_0000_0000_0000_0000_0001_0000 {
        // Undefined
        Und {cond: condition}
    }
    // ---------------- 27 = 1, 26 = 0 ----------------------
    else if   instr & 0b1110_0000_0000_0000_0000_0000_0000 == 0b1000_0000_0000_0000_0000_0000_0000 {
        // Block Data Transfer
        LdmStm {cond: condition, p: bits[24], u: bits[23], s: bits[22], w: bits[21], l: bits[20], rn: nibbles[4], regs: instr & 0xffff}
    } else if instr & 0b1110_0000_0000_0000_0000_0000_0000 == 0b1010_0000_0000_0000_0000_0000_0000 {
        // Branch
        B {cond: condition, l: bits[24], offset: instr & 0xffffff}
    }
    // ---------------- 27 = 1, 26 = 1 ----------------------
    else if  instr & 0b1110_0000_0000_0000_0000_0000_0000 == 0b1100_0000_0000_0000_0000_0000_0000 {
        // Coprocessor Data Transfer
        LdcStc {cond: condition, p: bits[24], u: bits[23], n: bits[22], w: bits[21], l: bits[20], rn: nibbles[4], crd: nibbles[3], cn: nibbles[2], offset: instr & 0xff}
    } else if instr & 0b1111_0000_0000_0000_0000_0001_0000 == 0b1110_0000_0000_0000_0000_0000_0000 {
        // Coprocessor Data Operation
        Cdp {cond: condition, c_opc: nibbles[5], crn: nibbles[4], crd: nibbles[3], cn: nibbles[2], cinf: nibbles[1] >> 1, crm: nibbles[0]}
    } else if instr & 0b1111_0000_0000_0000_0000_0001_0000 == 0b1110_0000_0000_0000_0000_0001_0000 {
        // Coprocessor Register Transfer
        McrMrc {cond: condition, copc: nibbles[5] >> 1, l: bits[20], crn: nibbles[4], rd: nibbles[3], cn: nibbles[2], cinf: nibbles[1] >> 1, crm: nibbles[0]}
    } else if instr & 0b1111_0000_0000_0000_0000_0000_0000 == 0b1111_0000_0000_0000_0000_0000_0000 {
        // Software Interrupt
        Swi {cond: condition, comment: instr & 0xffffff}
    } else {
        panic!()
    }
}


#[derive(Debug)]
pub enum Instruction {
    And{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 AND operand2
    Eor{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 EOR operand2 (XOR)
    Sub{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 - operand2
    Rsb{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand2 - operand1
    Add{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 + operand2
    Adc{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 + operand2 + carry
    Sbc{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 - operand2 + carry - 1
    Rsc{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand2 - operand1 + carry - 1
    Tst{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // as AND, but result is not written
    Teq{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // as EOR, but result is not written
    Cmp{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // as SUB, but result is not written
    Cmn{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // as ADD, but result is not written
    Orr{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 OR operand2
    Mov{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand2 (operand1 is ignored)
    Bic{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // operand1 AND NOT operand2 (Bit clear)
    Mvn{
        cond: Condition,
        i: bool,
        s: bool,
        rn: u32,
        rd: u32,
        op2: u32,
    }, // NOT operand2 (operand1 is ignored)
    B{
        cond: Condition,
        l: bool,
        offset: u32,
    },  // Branch, Branch with Link | R15 := address, R14 := R15, R15 := address
    Bx{
        cond: Condition,
        rn: u32,
    },  // Branch and Exchange | R15 := Rn, T bit := Rn[0]
    Cdp{
        cond: Condition,
        c_opc: u32,
        crn: u32,
        crd: u32,
        cn: u32,
        cinf: u32,
        crm: u32,
    }, // Coprocesor Data Processing | (Coprocessor-specific)
    LdcStc{
        cond: Condition,
        p: bool,
        u: bool,
        n: bool,
        w: bool,
        l: bool,
        rn: u32,
        crd: u32,
        cn: u32,
        offset: u32,
    }, // Load coprocessor from memory | Coprocessor load
       // Store coprocessor register to memory | address := CRn
    LdmStm{
        cond: Condition,
        p: bool,
        u: bool,
        s: bool,
        w: bool,
        l: bool,
        rn: u32,
        regs: u32,
    }, // Load multiple registers | Stack manipulation (Pop)
       // Store Multiple | Stack manipulation (Push)
    LdrStr{
        cond: Condition,
        i: bool,
        p: bool,
        u: bool,
        b: bool,
        w: bool,
        l: bool,
        rn: u32,
        rd: u32,
        offset: u32,
    }, // Load register from memory | Rd := (address)
       // Store register to memory | <address> := Rd
    LdrStrHalf{
        cond: Condition,
        p: bool,
        u: bool,
        i: bool, // custom
        w: bool,
        l: bool,
        rn: u32,
        rd: u32,
        s: bool,
        h: bool,
        offset: u32,
    },
    McrMrc {
        cond: Condition,
        copc: u32,
        l: bool,
        crn: u32,
        rd: u32,
        cn: u32,
        cinf: u32,
        crm: u32,
    }, // Move CPU register to coprocessor register | cRn := rRn {<op>cRm}
       // Move from coprocessor register to CPU register | Rn := cRn {<op>cRm}
    Mrs{
        cond: Condition,
        ps: bool,
        rd: u32,
    }, // Move PSR status/flags to register | Rn := PSR
    Msr{
        cond: Condition,
        i: bool,
        pd: bool,
        f: bool, // custom: PSR flag bits only
        op: u32,
    }, // Move register to PSR status/flags | PSR := Rm
    Mul{
        cond: Condition,
        a: bool,
        s: bool,
        rd: u32,
        rn: u32,
        rs: u32,
        rm: u32,
    }, // Multiply, Multiply Accumulate | Rd := Rm * Rs, Rd := (Rm * Rs) + Rn
    Mull{
        cond: Condition,
        u: bool,
        a: bool,
        s: bool,
        rd_hi: u32,
        rd_lo: u32,
        rn: u32,
        rm: u32,
    }, // Multiply Long Accumulate
    Swi{
        cond: Condition,
        comment: u32,
    }, // Software Interrupt | OS call
    Swp{
        cond: Condition,
        b: bool,
        rn: u32,
        rd: u32,
        rm: u32,
    }, // Swap register with memory | Rd := [Rn], [Rn] := Rm
    Und{
        cond: Condition,
    }, // Undefined
}
