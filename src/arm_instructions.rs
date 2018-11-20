use super::cpu::ARM7TDMI;
use super::registers::{Register, PsrBit};

impl ARM7TDMI {
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

    // ------------------------------------------------------------------------- //

    // TODO: return instruction execution time
    pub fn execute_arm(&mut self, instruction: Instruction) {
        use self::Instruction::*;
        match instruction {
            B{cond, l, offset} => self.exec_arm_b(cond, l, offset),
            Cmp{cond, i, s, rn, rd, op2} => self.exec_arm_cmp(cond, i, s, rn, rd, op2),
            _ => panic!("unimplemented! {:?}", instruction),
        }

        let old_pc = self.registers.read(Register::Pc);
        self.registers.write(old_pc + 4, Register::Pc);
    }

    // ------------------------------------------------------------------------- //

    // TODO: two's complement
    fn exec_arm_b(&mut self, cond: Condition, l: bool, offset: u32) {
        if !self.arm_condition_true(cond) {return}

        let old_pc = self.registers.read(Register::Pc) + 4;
        if l {
            self.registers.write(old_pc, Register::Lr);
        }
        self.registers.write(old_pc + (offset << 2), Register::Pc);
    }

    // TODO: incomplete
    fn exec_arm_cmp(&mut self, cond: Condition, i: bool, s: bool, rn: u32, rd: u32, op2: u32) {
        use self::PsrBit::*;
        if !self.arm_condition_true(cond) {return}

        if i  {
            // immediate value with rotate
        } else {
            // register with shift
        }

        let cmp = self.registers.index(rn) - op2;

        if s {
            let v = cmp >> 31 == 1;
            let c = cmp >> 31 == 1; // TODO: wrong
            let z = cmp == 0;
            let n = cmp >> 31 == 1; // ?
            self.registers.write_cpsr_bits(vec![(V, v), (C, c), (Z, z), (N, n)]);
            println!("{}", self.registers.cpsr.val);
        }
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
        Msr {cond: condition, pd: bits[22], op: instr & 0xfff}
    } else if instr & 0b1101_1011_1111_1111_0000_0000_0000 == 0b0001_0010_1000_1111_0000_0000_0000 {
        // MSR (transfer register contents or immdiate value to PSR flag bits only)
        Msr {cond: condition, pd: bits[22], op: instr & 0xfff}
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
        pd: bool,
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
