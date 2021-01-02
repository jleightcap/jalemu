use std::io::{Error, ErrorKind};

pub const RAM_SIZE: usize = 0x8000; // 32kB
pub const ROM_SIZE: usize = 0x8000; // 32kB
pub struct Cpu {
    /* registers */
    a:      u8,             // af pair
    f:      u8,             // dual-purpose flags
    b:      u8,             // bc pair
    c:      u8,
    d:      u8,             // de pair
    e:      u8,
    h:      u8,
    l:      u8,
    ix:     u16,
    iy:     u16,
    pc:     usize,          // program counter
    sp:     u16,            // stack pointer

    /* memory */
    ram:    [u8; RAM_SIZE],
    rom:    [u8; ROM_SIZE],
}

// registers
#[derive(Copy, Clone, Debug, PartialEq)]
enum R {
    A, F, B, C, D, E
}
// sandwich registers, read/write 2x8-bit registers as 1x16-bit register
#[derive(Copy, Clone, Debug, PartialEq)]
enum SR {
    AF, BC, DE, HL
}

// flags, stored as bits in F register
enum Flag {
    S, Z, /* gap */ H, /* gap */ PV, N, C
}

// program counter states
enum PC {
    I,              // increment by PC instruction length
    J(usize),       // jump
}

/* INSTRUCTIONS {{{ */
// methods if indexing memory
#[derive(Debug, PartialEq)]
enum MemAddr {
    Imm,            // (**)
    Reg(SR),        // (HL), (BC), etc.
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
}

#[derive(Debug, PartialEq)]
enum Instr {
    NOP,
    ADD16   (Arg16, Arg16),
    DEC8    (Arg8),
    DEC16   (Arg16),
    INC8    (Arg8),
    INC16   (Arg16),
    LD8     (Arg8, Arg8),
    LD16    (Arg16, Arg16),
    RLCA,
    RRCA,
}
/* }}} */


impl Cpu {
    pub fn new() -> Self {
        Cpu {
            a:      0x00,
            f:      0x00,
            b:      0x00,
            c:      0x00,
            d:      0x00,
            e:      0x00,
            h:      0x00,
            l:      0x00,
            ix:     0x0000,
            iy:     0x0000,
            pc:     0x0000,
            sp:     0x0000,

            ram:    [0x00; RAM_SIZE],
            rom:    [0x00; ROM_SIZE],
        }
    }

    /* SANDWICH REGS {{{ */
    // read from a sandwich register
    fn srr(&self, r: SR) -> u16 {
        match r {
            SR::AF => ((self.a as u16) << 8) | self.f as u16,
            SR::BC => ((self.b as u16) << 8) | self.c as u16,
            SR::DE => ((self.d as u16) << 8) | self.e as u16,
            SR::HL => ((self.h as u16) << 8) | self.l as u16,
        }
    }

    // write to a sandwich register
    fn srw(&mut self, r: SR, x: u16) {
        match r {
            SR::AF => {
                self.a = ((x & 0xff00) >> 8) as u8;
                self.f = ((x & 0x00ff) >> 0) as u8;
            },
            SR::BC => {
                self.b = ((x & 0xff00) >> 8) as u8;
                self.c = ((x & 0x00ff) >> 0) as u8;
            },
            SR::DE => {
                self.d = ((x & 0xff00) >> 8) as u8;
                self.e = ((x & 0x00ff) >> 0) as u8;
            },
            SR::HL => {
                self.h = ((x & 0xff00) >> 8) as u8;
                self.l = ((x & 0x00ff) >> 0) as u8;
            },
        }
    }
    /* }}} */

    /* FLAGS {{{ */
    fn fr(&self, f: Flag) -> bool {
        fn tob(u: u8) -> bool {
            match u {
                0 => false,
                1 => true,
                _ => panic!("bad flag extract logic"),
            }
        }
        match f {
            Flag::C  => tob((self.f & 0b00000001) >> 0),
            Flag::N  => tob((self.f & 0b00000010) >> 1),
            Flag::PV => tob((self.f & 0b00000100) >> 2),
            Flag::H  => tob((self.f & 0b00010000) >> 4),
            Flag::Z  => tob((self.f & 0b01000000) >> 6),
            Flag::S  => tob((self.f & 0b10000000) >> 7),
        }
    }

    fn fw(&mut self, f: Flag, b: bool) {
        fn bot(b: bool) -> u8 {
            match b {
                false => 0,
                true  => 1,
            }
        }
        match f {
            Flag::C  => {
                self.f &= !(1 << 0);
                println!("{:0>8b}", self.f);
                self.f |=  bot(b) << 0;
                println!("{:0>8b}", self.f);
            }
            Flag::N  => {
                self.f &= !(1 << 1);
                self.f |= bot(b) << 1;
            }
            Flag::PV => {
                self.f &= !(1 << 2);
                self.f |= bot(b) << 2;
            }
            Flag::H  => {
                self.f &= !(1 << 4);
                self.f |= bot(b) << 4;
            }
            Flag::Z  => {
                self.f &= !(1 << 6);
                self.f |= bot(b) << 6;
            }
            Flag::S  => {
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
            _ => Err(Error::new(ErrorKind::InvalidData, "write outside memory"))
        }

    }

    fn read(&self, x: usize) -> Result<u8, Error> {
        match x {
            0x0000..=0x7fff => Ok(self.rom[x]),
            0x8000..=0xffff => Ok(self.ram[x - 0x8000]),
            _ => Err(Error::new(ErrorKind::InvalidData, "read outside memory"))
        }
    }
    /* }}} */

    fn reset(&mut self) {
        self.a      = 0x00;
        self.f      = 0x00;
        self.b      = 0x00;
        self.c      = 0x00;
        self.d      = 0x00;
        self.e      = 0x00;
        self.h      = 0x00;
        self.l      = 0x00;
        self.ix     = 0x0000;
        self.iy     = 0x0000;
        self.pc     = 0x0000;
        self.sp     = 0x0000;

        self.ram    = [0x00; RAM_SIZE];
        //self.rom    = [0x00; ROM_SIZE];
    }

    fn fetch(&self) -> Result<u8, Error> {
        self.read(self.pc)
    }

    fn decode(&self, instr: u8) -> Result<Instr, Error> {
        match instr {
            0x00 => Ok(Instr::NOP),
            0x01 => Ok(Instr::LD16(
                        Arg16::Reg(SR::BC),
                        Arg16::U16
                    )),
            0x02 => Ok(Instr::LD8(
                        Arg8::Mem(MemAddr::Reg(SR::BC)),
                        Arg8::Reg(R::A)
                    )),
            0x03 => Ok(Instr::INC16(
                        Arg16::Reg(SR::BC)
                    )),
            0x04 => Ok(Instr::INC8(
                        Arg8::Reg(R::B)
                    )),
            0x05 => Ok(Instr::DEC8(
                        Arg8::Reg(R::B)
                    )),
            0x06 => Ok(Instr::LD8(
                        Arg8::Reg(R::B),
                        Arg8::U8
                    )),
            0x07 => Ok(Instr::RLCA),
            0x08 => panic!("TODO: ex af, af'"),
            0x09 => Ok(Instr::ADD16(
                        Arg16::Reg(SR::HL),
                        Arg16::Reg(SR::BC)
                    )),
            0x0a => Ok(Instr::LD8(
                        Arg8::Reg(R::A),
                        Arg8::Mem(MemAddr::Reg(SR::BC))
                    )),
            0x0b => Ok(Instr::DEC16(
                        Arg16::Reg(SR::BC)
                    )),
            0x0c => Ok(Instr::INC8(
                        Arg8::Reg(R::C)
                    )),
            0x0d => Ok(Instr::DEC8(
                        Arg8::Reg(R::C)
                    )),
            0x0e => Ok(Instr::LD8(
                        Arg8::Reg(R::C),
                        Arg8::U8
                    )),
            0x0f => Ok(Instr::RRCA),

            _ => Err(Error::new(ErrorKind::InvalidData, "unexpected opcode")),
        }
    }
}

#[cfg(test)]
#[path = "test/cpu_test.rs"]
mod cpu_test;

/* vim: set fdm=marker : */
