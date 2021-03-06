use std::io::{Error, ErrorKind};

pub const RAM_SIZE: usize = 0x8000; // 32kB
pub const ROM_SIZE: usize = 0x8000; // 32kB
pub struct Cpu {
    /* registers */
    a: u8, // af pair
    f: u8, // dual-purpose flags
    b: u8, // bc pair
    c: u8,
    d: u8, // de pair
    e: u8,
    h: u8,
    l: u8,
    ix: u16,
    iy: u16,
    pc: usize, // program counter
    sp: u16,   // stack pointer

    /* memory */
    ram: [u8; RAM_SIZE],
    rom: [u8; ROM_SIZE],
}

// registers
#[derive(Copy, Clone, Debug, PartialEq)]
enum R {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}
// sandwich registers, read/write 2x8-bit registers as 1x16-bit register
#[derive(Copy, Clone, Debug, PartialEq)]
enum SR {
    AF,
    BC,
    DE,
    HL,
    SP,
}

// flags, stored as bits in F register
#[derive(Debug, PartialEq)]
enum Flag {
    S,
    Z,
    /* gap */ H,
    /* gap */ PV,
    N,
    C,
}

// program counter states
#[derive(Debug, PartialEq)]
enum PC {
    I,         // increment by PC instruction length
    Im8,       // 8-bit immediate
    Im16,      // 16-bit immediate
    J(usize),  // absolute
    JR(usize), // relative jump
}

/* INSTRUCTIONS {{{ */

// indexing memory
#[derive(Debug, PartialEq)]
enum MemAddr {
    Imm,     // (**)
    Reg(SR), // (HL), (BC), etc.
}

// opcode argument with 8-bit length, examples:
// AND R
//     |
//     +------ 8-bit register argument
//
// LD R, *
//    |  |
//    |  +---- u8 immediate argument
//    +------- 8-bit register argument
//
// LD (**), A
//    |     |
//    |     +- 8-bit register argument
//    +------- u16 memory index
#[derive(Debug, PartialEq)]
enum Arg8 {
    U8,
    Reg(R),
    Mem(MemAddr),
}

// opcode argument with 16-bit length, examples:
// DEC HL
//     |
//     +------ 16-bit register argument
// LD HL, **
//    |   |
//    |   +--- u16 immediate argument
//    +------- 16-bit register argument
#[derive(Debug, PartialEq)]
enum Arg16 {
    U16,
    Reg(SR),
    Mem(MemAddr),
}

// opcode argument for flags
#[derive(Debug, PartialEq)]
enum ArgF {
    True,     // always (jr *)
    F(Flag),  // conditional (jr z, *)
    NF(Flag), // opposite conditonal (jr nz, *)
}

#[derive(Debug, PartialEq)]
enum Instr {
    ADC8(Arg8, Arg8),
    ADD8(Arg8, Arg8),
    ADD16(Arg16, Arg16),
    AND8(Arg8),
    CCF,
    CP8(Arg8),
    CPL,
    DAA,
    DEC8(Arg8),
    DEC16(Arg16),
    DJNZ(Arg8),
    EX(Arg16, Arg16),
    HALT,
    INC8(Arg8),
    INC16(Arg16),
    JR(ArgF, Arg8),
    LD8(Arg8, Arg8),
    LD16(Arg16, Arg16),
    NOP,
    OR8(Arg8),
    RLA,
    RLCA,
    RRA,
    RRCA,
    SBC8(Arg8, Arg8),
    SCF,
    SUB8(Arg8),
    XOR8(Arg8),
}
/* }}} */

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            a: 0x00,
            f: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            h: 0x00,
            l: 0x00,
            ix: 0x0000,
            iy: 0x0000,
            pc: 0x0000,
            sp: 0x0000,

            ram: [0x00; RAM_SIZE],
            rom: [0x00; ROM_SIZE],
        }
    }

    fn reset(&mut self) {
        self.a = 0x00;
        self.f = 0x00;
        self.b = 0x00;
        self.c = 0x00;
        self.d = 0x00;
        self.e = 0x00;
        self.h = 0x00;
        self.l = 0x00;
        self.ix = 0x0000;
        self.iy = 0x0000;
        self.pc = 0x0000;
        self.sp = 0x0000;

        self.ram = [0x00; RAM_SIZE];
    }

    /* REGS {{{ */
    // read from a register
    fn rr(&self, r: &R) -> u8 {
        match r {
            R::A => self.a,
            R::F => self.f,
            R::B => self.b,
            R::C => self.c,
            R::D => self.d,
            R::E => self.e,
            R::H => self.h,
            R::L => self.l,
        }
    }

    // write to a register
    fn rw(&mut self, r: &R, v: u8) {
        match r {
            R::A => self.a = v,
            R::F => self.f = v,
            R::B => self.b = v,
            R::C => self.c = v,
            R::D => self.d = v,
            R::E => self.e = v,
            R::H => self.h = v,
            R::L => self.l = v,
        }
    }

    // read from a sandwich register
    fn srr(&self, r: &SR) -> u16 {
        match r {
            SR::AF => ((self.a as u16) << 8) | self.f as u16,
            SR::BC => ((self.b as u16) << 8) | self.c as u16,
            SR::DE => ((self.d as u16) << 8) | self.e as u16,
            SR::HL => ((self.h as u16) << 8) | self.l as u16,
            SR::SP => self.sp,
        }
    }

    // write to a sandwich register
    #[cfg(not(tarpaulin_include))]
    fn srw(&mut self, r: &SR, x: u16) {
        match r {
            SR::AF => {
                self.a = ((x & 0xff00) >> 8) as u8;
                self.f = ((x & 0x00ff) >> 0) as u8;
            }
            SR::BC => {
                self.b = ((x & 0xff00) >> 8) as u8;
                self.c = ((x & 0x00ff) >> 0) as u8;
            }
            SR::DE => {
                self.d = ((x & 0xff00) >> 8) as u8;
                self.e = ((x & 0x00ff) >> 0) as u8;
            }
            SR::HL => {
                self.h = ((x & 0xff00) >> 8) as u8;
                self.l = ((x & 0x00ff) >> 0) as u8;
            }
            SR::SP => {
                self.sp = x;
            }
        }
    }
    /* }}} */

    /* FLAGS {{{ */
    #[cfg(not(tarpaulin_include))]
    fn fr(&self, f: &Flag) -> bool {
        fn tob(u: u8) -> bool {
            match u {
                0 => false,
                1 => true,
                _ => panic!("bad flag extract logic"),
            }
        }
        match f {
            Flag::C => tob((self.f & 0b00000001) >> 0),
            Flag::N => tob((self.f & 0b00000010) >> 1),
            Flag::PV => tob((self.f & 0b00000100) >> 2),
            Flag::H => tob((self.f & 0b00010000) >> 4),
            Flag::Z => tob((self.f & 0b01000000) >> 6),
            Flag::S => tob((self.f & 0b10000000) >> 7),
        }
    }

    #[cfg(not(tarpaulin_include))]
    fn fw(&mut self, f: &Flag, b: bool) {
        fn bot(b: bool) -> u8 {
            if b {
                1
            } else {
                0
            }
        }
        match f {
            Flag::C => {
                self.f &= !(1 << 0);
                self.f |= bot(b) << 0;
            }
            Flag::N => {
                self.f &= !(1 << 1);
                self.f |= bot(b) << 1;
            }
            Flag::PV => {
                self.f &= !(1 << 2);
                self.f |= bot(b) << 2;
            }
            Flag::H => {
                self.f &= !(1 << 4);
                self.f |= bot(b) << 4;
            }
            Flag::Z => {
                self.f &= !(1 << 6);
                self.f |= bot(b) << 6;
            }
            Flag::S => {
                self.f &= !(1 << 7);
                self.f |= bot(b) << 7;
            }
        }
    }
    /* }}} */

    /* MEMORY {{{ */
    fn write(&mut self, x: usize, v: u8) -> Result<(), Error> {
        match x {
            0x0000..=0x7fff => Ok(()), // write to ROM has no action
            0x8000..=0xffff => Ok(self.ram[x - 0x8000] = v),
            _ => Err(Error::new(ErrorKind::InvalidData, "write outside memory")),
        }
    }

    fn read(&self, x: usize) -> Result<u8, Error> {
        match x {
            0x0000..=0x7fff => Ok(self.rom[x]),
            0x8000..=0xffff => Ok(self.ram[x - 0x8000]),
            _ => Err(Error::new(ErrorKind::InvalidData, "read outside memory")),
        }
    }

    fn imm8(&self) -> Result<u8, Error> {
        let lo = self.read(self.pc + 1)?;
        Ok(lo)
    }

    fn imm16(&self) -> Result<u16, Error> {
        let (hi, lo) = (self.read(self.pc + 1)?, self.read(self.pc + 2)?);
        Ok(((hi as u16) << 8) | lo as u16)
    }
    /* }}} */

    fn fetch(&self) -> Result<u8, Error> {
        self.read(self.pc)
    }

    /* DECODE {{{ */
    fn decode(&self, instr: u8) -> Result<Instr, Error> {
        match instr {
            /* 0x0X {{{ */
            0x00 => Ok(Instr::NOP),
            0x01 => Ok(Instr::LD16(Arg16::Reg(SR::BC), Arg16::U16)),
            0x02 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::BC)), Arg8::Reg(R::A))),
            0x03 => Ok(Instr::INC16(Arg16::Reg(SR::BC))),
            0x04 => Ok(Instr::INC8(Arg8::Reg(R::B))),
            0x05 => Ok(Instr::DEC8(Arg8::Reg(R::B))),
            0x06 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::U8)),
            0x07 => Ok(Instr::RLCA),
            0x08 => Ok(Instr::EX(
                Arg16::Reg(SR::AF),
                Arg16::Reg(SR::AF), // TODO: shadow registers
            )),
            0x09 => Ok(Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::BC))),
            0x0a => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::BC)))),
            0x0b => Ok(Instr::DEC16(Arg16::Reg(SR::BC))),
            0x0c => Ok(Instr::INC8(Arg8::Reg(R::C))),
            0x0d => Ok(Instr::DEC8(Arg8::Reg(R::C))),
            0x0e => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::U8)),
            0x0f => Ok(Instr::RRCA),
            /* }}} */
            /* 0x1X {{{ */
            0x10 => Ok(Instr::DJNZ(Arg8::U8)),
            0x11 => Ok(Instr::LD16(Arg16::Reg(SR::DE), Arg16::U16)),
            0x12 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::DE)), Arg8::Reg(R::A))),
            0x13 => Ok(Instr::INC16(Arg16::Reg(SR::DE))),
            0x14 => Ok(Instr::INC8(Arg8::Reg(R::D))),
            0x15 => Ok(Instr::DEC8(Arg8::Reg(R::D))),
            0x16 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::U8)),
            0x17 => Ok(Instr::RLA),
            0x18 => Ok(Instr::JR(ArgF::True, Arg8::U8)),
            0x19 => Ok(Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::DE))),
            0x1a => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::DE)))),
            0x1b => Ok(Instr::DEC16(Arg16::Reg(SR::DE))),
            0x1c => Ok(Instr::INC8(Arg8::Reg(R::E))),
            0x1d => Ok(Instr::DEC8(Arg8::Reg(R::E))),
            0x1e => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::U8)),
            0x1f => Ok(Instr::RRA),
            /* }}} */
            /* 0x2X {{{ */
            0x20 => Ok(Instr::JR(ArgF::NF(Flag::Z), Arg8::U8)),
            0x21 => Ok(Instr::LD16(Arg16::Reg(SR::HL), Arg16::U16)),
            0x22 => Ok(Instr::LD16(Arg16::Mem(MemAddr::Imm), Arg16::Reg(SR::HL))),
            0x23 => Ok(Instr::INC16(Arg16::Reg(SR::HL))),
            0x24 => Ok(Instr::INC8(Arg8::Reg(R::H))),
            0x25 => Ok(Instr::DEC8(Arg8::Reg(R::H))),
            0x26 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::U8)),
            0x27 => Ok(Instr::DAA),
            0x28 => Ok(Instr::JR(ArgF::F(Flag::Z), Arg8::U8)),
            0x29 => Ok(Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::HL))),
            0x2a => Ok(Instr::LD16(Arg16::Reg(SR::HL), Arg16::Mem(MemAddr::Imm))),
            0x2b => Ok(Instr::DEC16(Arg16::Reg(SR::HL))),
            0x2c => Ok(Instr::INC8(Arg8::Reg(R::L))),
            0x2d => Ok(Instr::DEC8(Arg8::Reg(R::L))),
            0x2e => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::U8)),
            0x2f => Ok(Instr::CPL),
            /* }}} */
            /* 0x3X {{{ */
            0x30 => Ok(Instr::JR(ArgF::NF(Flag::C), Arg8::U8)),
            0x31 => Ok(Instr::LD16(Arg16::Reg(SR::SP), Arg16::U16)),
            0x32 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Imm), Arg8::Reg(R::A))),
            0x33 => Ok(Instr::INC16(Arg16::Reg(SR::SP))),
            0x34 => Ok(Instr::INC8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x35 => Ok(Instr::DEC8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x36 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::U8)),
            0x37 => Ok(Instr::SCF),
            0x38 => Ok(Instr::JR(ArgF::F(Flag::C), Arg8::U8)),
            0x39 => Ok(Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::SP))),
            0x3a => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Imm))),
            0x3b => Ok(Instr::DEC16(Arg16::Reg(SR::SP))),
            0x3c => Ok(Instr::INC8(Arg8::Reg(R::A))),
            0x3d => Ok(Instr::DEC8(Arg8::Reg(R::A))),
            0x3e => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::U8)),
            0x3f => Ok(Instr::CCF),
            /* }}} */
            /* 0x4X {{{ */
            0x40 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::B))),
            0x41 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::C))),
            0x42 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::D))),
            0x43 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::E))),
            0x44 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::H))),
            0x45 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::L))),
            0x46 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x47 => Ok(Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::A))),
            0x48 => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::B))),
            0x49 => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::C))),
            0x4a => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::D))),
            0x4b => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::E))),
            0x4c => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::H))),
            0x4d => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::L))),
            0x4e => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x4f => Ok(Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::A))),
            /* }}} */
            /* 0x5X {{{ */
            0x50 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::B))),
            0x51 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::C))),
            0x52 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::D))),
            0x53 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::E))),
            0x54 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::H))),
            0x55 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::L))),
            0x56 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x57 => Ok(Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::A))),
            0x58 => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::B))),
            0x59 => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::C))),
            0x5a => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::D))),
            0x5b => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::E))),
            0x5c => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::H))),
            0x5d => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::L))),
            0x5e => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x5f => Ok(Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::A))),
            /* }}} */
            /* 0x6X {{{ */
            0x60 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::B))),
            0x61 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::C))),
            0x62 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::D))),
            0x63 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::E))),
            0x64 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::H))),
            0x65 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::L))),
            0x66 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x67 => Ok(Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::A))),
            0x68 => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::B))),
            0x69 => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::C))),
            0x6a => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::D))),
            0x6b => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::E))),
            0x6c => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::H))),
            0x6d => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::L))),
            0x6e => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x6f => Ok(Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::A))),
            /* }}} */
            /* 0x7X {{{ */
            0x70 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::B))),
            0x71 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::C))),
            0x72 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::D))),
            0x73 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::E))),
            0x74 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::H))),
            0x75 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::L))),
            0x76 => Ok(Instr::HALT),
            0x77 => Ok(Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::A))),
            0x78 => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::B))),
            0x79 => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::C))),
            0x7a => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::D))),
            0x7b => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::E))),
            0x7c => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::H))),
            0x7d => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::L))),
            0x7e => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x7f => Ok(Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::A))),
            /* }}} */
            /* 0x8X {{{ */
            0x80 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::B))),
            0x81 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::C))),
            0x82 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::D))),
            0x83 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::E))),
            0x84 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::H))),
            0x85 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::L))),
            0x86 => Ok(Instr::ADD8(
                Arg8::Reg(R::A),
                Arg8::Mem(MemAddr::Reg(SR::HL)),
            )),
            0x87 => Ok(Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::A))),
            0x88 => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::B))),
            0x89 => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::C))),
            0x8a => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::D))),
            0x8b => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::E))),
            0x8c => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::H))),
            0x8d => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::L))),
            0x8e => Ok(Instr::ADC8(
                Arg8::Reg(R::A),
                Arg8::Mem(MemAddr::Reg(SR::HL)),
            )),
            0x8f => Ok(Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::A))),
            /* }}} */
            /* 0x9X {{{ */
            0x90 => Ok(Instr::SUB8(Arg8::Reg(R::B))),
            0x91 => Ok(Instr::SUB8(Arg8::Reg(R::C))),
            0x92 => Ok(Instr::SUB8(Arg8::Reg(R::D))),
            0x93 => Ok(Instr::SUB8(Arg8::Reg(R::E))),
            0x94 => Ok(Instr::SUB8(Arg8::Reg(R::H))),
            0x95 => Ok(Instr::SUB8(Arg8::Reg(R::L))),
            0x96 => Ok(Instr::SUB8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0x97 => Ok(Instr::SUB8(Arg8::Reg(R::A))),
            0x98 => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::B))),
            0x99 => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::C))),
            0x9a => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::D))),
            0x9b => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::E))),
            0x9c => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::H))),
            0x9d => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::L))),
            0x9e => Ok(Instr::SBC8(
                Arg8::Reg(R::A),
                Arg8::Mem(MemAddr::Reg(SR::HL)),
            )),
            0x9f => Ok(Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::A))),
            /* }}} */
            /* 0xaX {{{ */
            0xa0 => Ok(Instr::AND8(Arg8::Reg(R::B))),
            0xa1 => Ok(Instr::AND8(Arg8::Reg(R::C))),
            0xa2 => Ok(Instr::AND8(Arg8::Reg(R::D))),
            0xa3 => Ok(Instr::AND8(Arg8::Reg(R::E))),
            0xa4 => Ok(Instr::AND8(Arg8::Reg(R::H))),
            0xa5 => Ok(Instr::AND8(Arg8::Reg(R::L))),
            0xa6 => Ok(Instr::AND8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0xa7 => Ok(Instr::AND8(Arg8::Reg(R::A))),
            0xa8 => Ok(Instr::XOR8(Arg8::Reg(R::B))),
            0xa9 => Ok(Instr::XOR8(Arg8::Reg(R::C))),
            0xaa => Ok(Instr::XOR8(Arg8::Reg(R::D))),
            0xab => Ok(Instr::XOR8(Arg8::Reg(R::E))),
            0xac => Ok(Instr::XOR8(Arg8::Reg(R::H))),
            0xad => Ok(Instr::XOR8(Arg8::Reg(R::L))),
            0xae => Ok(Instr::XOR8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0xaf => Ok(Instr::XOR8(Arg8::Reg(R::A))),
            /* }}} */
            /* 0xbX {{{ */
            0xb0 => Ok(Instr::OR8(Arg8::Reg(R::B))),
            0xb1 => Ok(Instr::OR8(Arg8::Reg(R::C))),
            0xb2 => Ok(Instr::OR8(Arg8::Reg(R::D))),
            0xb3 => Ok(Instr::OR8(Arg8::Reg(R::E))),
            0xb4 => Ok(Instr::OR8(Arg8::Reg(R::H))),
            0xb5 => Ok(Instr::OR8(Arg8::Reg(R::L))),
            0xb6 => Ok(Instr::OR8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0xb7 => Ok(Instr::OR8(Arg8::Reg(R::A))),
            0xb8 => Ok(Instr::CP8(Arg8::Reg(R::B))),
            0xb9 => Ok(Instr::CP8(Arg8::Reg(R::C))),
            0xba => Ok(Instr::CP8(Arg8::Reg(R::D))),
            0xbb => Ok(Instr::CP8(Arg8::Reg(R::E))),
            0xbc => Ok(Instr::CP8(Arg8::Reg(R::H))),
            0xbd => Ok(Instr::CP8(Arg8::Reg(R::L))),
            0xbe => Ok(Instr::CP8(Arg8::Mem(MemAddr::Reg(SR::HL)))),
            0xbf => Ok(Instr::CP8(Arg8::Reg(R::A))),
            /* }}} */
            _ => Err(Error::new(ErrorKind::InvalidData, "unexpected opcode")),
        }
    }
    /* }}} */

    fn execute(&mut self, instr: &Instr) -> Result<(), Error> {
        let pc = match instr {
            Instr::ADC8(a1, a2) => self.adc8(a1, a2),
            Instr::ADD8(a1, a2) => self.add8(a1, a2),
            Instr::ADD16(a1, a2) => self.add16(a1, a2),
            Instr::AND8(a) => self.and8(a),
            Instr::CCF => self.ccf(),
            Instr::CP8(a) => self.cp8(a),
            Instr::CPL => self.cpl(),
            Instr::DAA => self.daa(),
            Instr::DEC8(a) => self.dec8(a),
            Instr::DEC16(a) => self.dec16(a),
            Instr::DJNZ(a) => self.djnz(a),
            Instr::EX(a1, a2) => self.ex(a1, a2),
            Instr::HALT => self.halt(),
            Instr::INC8(a) => self.inc8(a),
            Instr::INC16(a) => self.inc16(a),
            Instr::JR(f, a) => self.jr(f, a),
            Instr::LD8(a1, a2) => self.ld8(a1, a2),
            Instr::LD16(a1, a2) => self.ld16(a1, a2),
            Instr::NOP => self.nop(),
            Instr::OR8(a) => self.or8(a),
            Instr::RLA => self.rla(),
            Instr::RLCA => self.rlca(),
            Instr::RRA => self.rra(),
            Instr::RRCA => self.rrca(),
            Instr::SBC8(a1, a2) => self.sbc8(a1, a2),
            Instr::SCF => self.scf(),
            Instr::SUB8(a) => self.sub8(a),
            Instr::XOR8(a) => self.xor8(a),
        }?;

        Ok(match pc {
            PC::I => self.pc = self.pc.wrapping_add(1) & 0xffff,
            PC::Im8 => self.pc = self.pc.wrapping_add(2) & 0xffff,
            PC::Im16 => self.pc = self.pc.wrapping_add(3) & 0xffff,
            PC::J(x) => self.pc = x,
            PC::JR(x) => self.pc = (self.pc as i8).wrapping_add(x as i8) as usize & 0xffff,
        })
    }

    /* OPCODES {{{ */
    // get the value of an 8-bit argument
    fn u8_arg(&self, a: &Arg8) -> Result<(u8, PC), Error> {
        match a {
            // op X, R
            Arg8::Reg(r) => Ok((self.rr(r), PC::I)),
            // op X, *
            Arg8::U8 => Ok((self.imm8()?, PC::Im8)),
            Arg8::Mem(addr) => match addr {
                // op X, (**)
                MemAddr::Imm => Ok((self.read(self.imm16()? as usize)?, PC::Im16)),
                // op X, (SR)
                MemAddr::Reg(sr) => Ok((self.read(self.srr(sr) as usize)?, PC::I)),
            },
        }
    }

    // get the value of a 16-bit argument
    fn u16_arg(&self, a: &Arg16) -> Result<(u16, PC), Error> {
        match a {
            Arg16::U16 => Ok((self.imm16()?, PC::Im16)),
            Arg16::Reg(sr) => Ok((self.srr(sr), PC::I)),
            Arg16::Mem(_) => Err(Error::new(
                ErrorKind::InvalidData,
                "16-bit arg size mismatch",
            )),
        }
    }

    fn adc8(&mut self, dst: &Arg8, src: &Arg8) -> Result<PC, Error> {
        fn bot(b: bool) -> u8 {
            match b {
                false => 0,
                true => 1,
            }
        }
        let (src, pc) = self.u8_arg(src)?;
        match dst {
            // adc R, X
            Arg8::Reg(r) => {
                self.rw(
                    r,
                    self.rr(r)
                        .wrapping_add(src)
                        .wrapping_add(bot(self.fr(&Flag::C))),
                );
                Ok(pc)
            }
            // adc *, X
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
            // adc [**], X
            Arg8::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
        }
    }

    fn add8(&mut self, dst: &Arg8, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        match dst {
            // add R, X
            Arg8::Reg(r) => {
                self.rw(r, self.rr(r).wrapping_add(src));
                Ok(pc)
            }
            // add *, X
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
            // add [**], X
            Arg8::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
        }
    }

    fn add16(&mut self, dst: &Arg16, src: &Arg16) -> Result<PC, Error> {
        let (src, pc) = self.u16_arg(src)?;
        match dst {
            // add SR, X
            Arg16::Reg(sr) => {
                self.srw(sr, self.srr(sr).wrapping_add(src));
                Ok(pc)
            }
            // add **, X
            Arg16::U16 => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
            // add [**], X
            Arg16::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
        }
    }

    fn and8(&mut self, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        // and A, X
        self.rw(&R::A, self.rr(&R::A) & src);
        Ok(pc)
    }

    fn ccf(&mut self) -> Result<PC, Error> {
        self.fw(&Flag::C, false);
        Ok(PC::I)
    }

    fn cp8(&mut self, src: &Arg8) -> Result<PC, Error> {
        // TODO: cp R
        Ok(PC::I)
    }

    fn cpl(&mut self) -> Result<PC, Error> {
        self.rw(&R::A, self.rr(&R::A) ^ 0xff); // 1's complement
        Ok(PC::I)
    }

    fn daa(&mut self) -> Result<PC, Error> {
        // TODO: BCD
        Ok(PC::I)
    }

    fn dec8(&mut self, dst: &Arg8) -> Result<PC, Error> {
        match dst {
            // dec R
            Arg8::Reg(r) => {
                self.rw(r, self.rr(r).wrapping_sub(1));
                Ok(PC::I)
            }
            // dec *
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "sub to immediate")),
            Arg8::Mem(addr) => match addr {
                // dec [**]
                MemAddr::Imm => Err(Error::new(ErrorKind::InvalidData, "sub to immediate")),
                // dec [SR]
                MemAddr::Reg(sr) => {
                    let val = self.read(self.srr(sr) as usize)?;
                    self.write(self.srr(sr) as usize, val.wrapping_sub(1))?;
                    Ok(PC::I)
                }
            },
        }
    }

    fn dec16(&mut self, dst: &Arg16) -> Result<PC, Error> {
        match dst {
            // dec SR
            Arg16::Reg(sr) => {
                self.srw(sr, self.srr(sr).wrapping_sub(1));
                Ok(PC::I)
            }
            // dec **
            Arg16::U16 => Err(Error::new(ErrorKind::InvalidData, "dec to immediate")),
            // dec [**]
            Arg16::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "dec from immediate")),
        }
    }

    fn djnz(&mut self, dst: &Arg8) -> Result<PC, Error> {
        match self.fr(&Flag::Z) {
            false => {
                let (dst, _) = self.u8_arg(dst)?;
                Ok(PC::JR(dst as usize))
            }
            true => Ok(PC::Im8),
        }
    }

    fn ex(&mut self, dst: &Arg16, src: &Arg16) -> Result<PC, Error> {
        // TODO: shadow registers
        Ok(PC::I)
    }

    fn halt(&mut self) -> Result<PC, Error> {
        // TODO: halt
        Ok(PC::I)
    }

    fn inc8(&mut self, dst: &Arg8) -> Result<PC, Error> {
        match dst {
            // inc R
            Arg8::Reg(r) => {
                self.rw(r, self.rr(r).wrapping_add(1));
                Ok(PC::I)
            }
            // inc *
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
            Arg8::Mem(addr) => match addr {
                // inc [**]
                MemAddr::Imm => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
                // inc [SR]
                MemAddr::Reg(sr) => {
                    let val = self.read(self.srr(sr) as usize)?;
                    self.write(self.srr(sr) as usize, val.wrapping_add(1))?;
                    Ok(PC::I)
                }
            },
        }
    }

    fn inc16(&mut self, dst: &Arg16) -> Result<PC, Error> {
        match dst {
            // inc SR
            Arg16::Reg(sr) => {
                self.srw(sr, self.srr(sr).wrapping_add(1));
                Ok(PC::I)
            }
            // inc **
            Arg16::U16 => Err(Error::new(ErrorKind::InvalidData, "inc to immediate")),
            // inc [**]
            Arg16::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "inc to immediate")),
        }
    }

    fn jr(&mut self, f: &ArgF, dst: &Arg8) -> Result<PC, Error> {
        let (dst, _) = self.u8_arg(dst)?;
        match f {
            ArgF::True => Ok(PC::JR(dst as usize)),
            ArgF::F(f) => {
                if self.fr(f) {
                    Ok(PC::JR(dst as usize))
                } else {
                    Ok(PC::Im8)
                }
            }
            ArgF::NF(f) => {
                if !self.fr(f) {
                    Ok(PC::JR(dst as usize))
                } else {
                    Ok(PC::Im8)
                }
            }
        }
    }

    fn ld8(&mut self, dst: &Arg8, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        match dst {
            // ld R, X
            Arg8::Reg(r) => {
                self.rw(r, src);
                Ok(pc)
            }
            // ld *, X
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "load to immediate")),
            Arg8::Mem(addr) => match addr {
                // ld (**), X
                MemAddr::Imm => Err(Error::new(ErrorKind::InvalidData, "load to immediate")),
                // ld (SR), X
                MemAddr::Reg(sr) => {
                    self.write(self.srr(sr) as usize, src)?;
                    Ok(pc)
                }
            },
        }
    }

    fn ld16(&mut self, dst: &Arg16, src: &Arg16) -> Result<PC, Error> {
        let (src, pc) = self.u16_arg(src)?;
        match dst {
            // ld SR, X
            Arg16::Reg(sr) => {
                self.srw(sr, src);
                Ok(pc)
            }
            // ld **, X
            Arg16::U16 => Err(Error::new(ErrorKind::InvalidData, "load to immediate")),
            // ld [..], X
            Arg16::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "load size mismatch")),
        }
    }

    fn nop(&mut self) -> Result<PC, Error> {
        Ok(PC::I)
    }

    fn or8(&mut self, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        // or A, X
        self.rw(&R::A, self.rr(&R::A) | src);
        Ok(pc)
    }

    fn rla(&mut self) -> Result<PC, Error> {
        fn bot(b: bool) -> u8 {
            match b {
                false => 0,
                true => 1,
            }
        }
        fn tob(t: u8) -> bool {
            match t {
                0 => false,
                1 => true,
                _ => panic!("bad flag"),
            }
        }
        let prev = self.rr(&R::A);
        let cf = self.fr(&Flag::C);
        // rotate left, previous C flag in LSB
        self.rw(&R::A, prev << 1 | bot(cf));
        // previous A MSB in C flag
        self.fw(&Flag::C, tob((prev >> 7) & 0b1));
        Ok(PC::I)
    }

    fn rlca(&mut self) -> Result<PC, Error> {
        let prev = self.rr(&R::A);
        let msb = (prev >> 7) & 0b1;
        self.rw(&R::A, prev << 1 | (msb << 0));
        Ok(PC::I)
    }

    fn rra(&mut self) -> Result<PC, Error> {
        fn bot(b: bool) -> u8 {
            match b {
                false => 0,
                true => 1,
            }
        }
        fn tob(t: u8) -> bool {
            match t {
                0 => false,
                1 => true,
                _ => panic!("bad flag"),
            }
        }
        let prev = self.rr(&R::A);
        let cf = self.fr(&Flag::C);
        // rotate right, previous C flag in MSB
        self.rw(&R::A, prev >> 1 | (bot(cf) << 7));
        // previous A LSB in C flag
        self.fw(&Flag::C, tob(prev & 0b1));
        Ok(PC::I)
    }

    fn rrca(&mut self) -> Result<PC, Error> {
        let prev = self.rr(&R::A);
        let lsb = (prev >> 0) & 0b1;
        self.rw(&R::A, prev >> 1 | (lsb << 7));
        Ok(PC::I)
    }

    fn sbc8(&mut self, dst: &Arg8, src: &Arg8) -> Result<PC, Error> {
        fn bot(b: bool) -> u8 {
            match b {
                false => 0,
                true => 1,
            }
        }
        let (src, pc) = self.u8_arg(src)?;
        match dst {
            // sbc R, X
            Arg8::Reg(r) => {
                self.rw(
                    r,
                    self.rr(r)
                        .wrapping_sub(src)
                        .wrapping_sub(bot(self.fr(&Flag::C))),
                );
                Ok(pc)
            }
            // sbc *, X
            Arg8::U8 => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
            // sbc [**], X
            Arg8::Mem(_) => Err(Error::new(ErrorKind::InvalidData, "add to immediate")),
        }
    }

    fn scf(&mut self) -> Result<PC, Error> {
        self.fw(&Flag::C, true);
        Ok(PC::I)
    }

    fn sub8(&mut self, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        // sub A, X
        self.rw(&R::A, self.rr(&R::A).wrapping_sub(src));
        Ok(pc)
    }

    fn xor8(&mut self, src: &Arg8) -> Result<PC, Error> {
        let (src, pc) = self.u8_arg(src)?;
        // xor A, X
        self.rw(&R::A, self.rr(&R::A) ^ src);
        Ok(pc)
    }

    /* }}} */
}

#[cfg(test)]
#[path = "test/cpu_test.rs"]
mod cpu_test;

/* vim: set fdm=marker : */
