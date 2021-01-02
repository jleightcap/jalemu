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
    ix:     u16,
    iy:     u16,
    pc:     usize,          // program counter
    sp:     u16,            // stack pointer

    /* memory */
    ram:    [u8; RAM_SIZE],
    rom:    [u8; ROM_SIZE],
}
enum SR {
    AF, BC, DE              // sandwich registers, read/write 2x8-bit registers as 16bit
}

impl Cpu {
    fn new() -> Self {
        Cpu {
            a:      0x00,
            f:      0x00,
            b:      0x00,
            c:      0x00,
            d:      0x00,
            e:      0x00,
            ix:     0x0000,
            iy:     0x0000,
            pc:     0x0000,
            sp:     0x0000,

            ram:    [0x00; RAM_SIZE],
            rom:    [0x00; ROM_SIZE],
        }
    }

    // read from a sandwich register
    fn srr(&self, r: SR) -> u16 {
        match r {
            SR::AF => ((self.a as u16) << 8) | self.f as u16,
            SR::BC => ((self.b as u16) << 8) | self.c as u16,
            SR::DE => ((self.d as u16) << 8) | self.e as u16,
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
        }
    }

    fn write(&mut self, x: usize, v: u8) -> Result<(), Error> {
        match x {
            0x0000..=0x7fff => Ok(self.rom[x] = v),
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

    fn fetch(&mut self) -> Result<u16, Error> {
        let hi = self.read(self.pc    )? as u16;
        let lo = self.read(self.pc + 1)? as u16;
        self.pc += 2;
        Ok((hi << 8) | lo)
    }
}

#[cfg(test)]
#[path = "test/cpu_test.rs"]
mod cpu_test;

/* vim: set fdm=marker : */
