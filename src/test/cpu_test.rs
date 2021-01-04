use super::*;

fn insert_imm8(c: &mut Cpu, imm8: u8) {
    c.rom[c.pc + 1] = imm8
}

fn insert_imm16(c: &mut Cpu, imm16: u16) {
    c.rom[c.pc + 1] = ((imm16 & 0xff00) >> 8) as u8;
    c.rom[c.pc + 2] = ((imm16 & 0x00ff) >> 0) as u8;
}

#[test]
fn test_read() -> Result<(), Error> {
    // valid reads
    let mut c = Cpu::new();
    c.rom[0x0000] = 0xde; // NOTE: don't actually write to memory like this,
    c.rom[0x0001] = 0xad; // just an independent test from self.write()
    assert_eq!(c.read(0x0000)?, 0xde);
    assert_eq!(c.read(0x0001)?, 0xad);
    c.ram[0x0000] = 0xbe; // similarly, ram shouldn't be written like this
    c.ram[0x0001] = 0xef; // ram[0x0000] mapped to 0x8000
    c.ram[0x7fff] = 0xff;
    assert_eq!(c.read(0x8000)?, 0xbe);
    assert_eq!(c.read(0x8001)?, 0xef);
    assert_eq!(c.read(0xffff)?, 0xff);

    // invalid reads
    let c = Cpu::new();
    let e = c.read(0x10000).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_write() -> Result<(), Error> {
    // valid writes
    let mut c = Cpu::new();
    c.write(0x8000, 0xbe)?;
    c.write(0x8001, 0xef)?;
    assert_eq!(c.ram[0x0000], 0xbe); // NOTE: don't actually read from memory like this,
    assert_eq!(c.ram[0x0001], 0xef); // just an independent test from self.read()
    c.write(0x0000, 0xde)?;
    c.write(0x0001, 0xad)?;
    assert_eq!(c.rom[0x0000], 0x00); // writes to ROM have no affect
    assert_eq!(c.rom[0x0001], 0x00);

    // invalid writes
    let mut c = Cpu::new();
    let e = c.write(0x10000, 0xff).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_insert_imm8() {
    let mut c = Cpu::new();
    insert_imm8(&mut c, 0xfe);
    assert_eq!(c.rom[0x0001], 0xfe);
}

#[test]
fn test_insert_imm16() {
    let mut c = Cpu::new();
    insert_imm16(&mut c, 0xbeef);
    assert_eq!(c.rom[0x0001], 0xbe);
    assert_eq!(c.rom[0x0002], 0xef);
}

#[test]
fn test_fetch() -> Result<(), Error> {
    // valid fetch
    let mut c = Cpu::new();
    c.rom[0x0000] = 0xde;
    c.rom[0x0001] = 0xad;
    assert_eq!(c.fetch()?, 0xde);
    c.pc += 1;
    assert_eq!(c.fetch()?, 0xad);

    // invalid fetch
    c.pc = 0x10000;
    let e = c.fetch().map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_register() {
    // normal registers
    let mut c = Cpu::new();
    c.a = 0x01;
    c.f = 0x02;
    c.b = 0x03;
    c.c = 0x04;
    c.d = 0x05;
    c.e = 0x06;
    c.h = 0x07;
    c.l = 0x08;

    assert_eq!(c.rr(&R::A), 0x01);
    c.rw(&R::A, 0x10);
    assert_eq!(c.a, 0x10);

    assert_eq!(c.rr(&R::F), 0x02);
    c.rw(&R::F, 0x20);
    assert_eq!(c.f, 0x20);

    assert_eq!(c.rr(&R::B), 0x03);
    c.rw(&R::B, 0x30);
    assert_eq!(c.b, 0x30);

    assert_eq!(c.rr(&R::C), 0x04);
    c.rw(&R::C, 0x40);
    assert_eq!(c.c, 0x40);

    assert_eq!(c.rr(&R::D), 0x05);
    c.rw(&R::D, 0x50);
    assert_eq!(c.d, 0x50);

    assert_eq!(c.rr(&R::E), 0x06);
    c.rw(&R::E, 0x60);
    assert_eq!(c.e, 0x60);

    assert_eq!(c.rr(&R::H), 0x07);
    c.rw(&R::H, 0x70);
    assert_eq!(c.h, 0x70);

    assert_eq!(c.rr(&R::L), 0x08);
    c.rw(&R::L, 0x80);
    assert_eq!(c.l, 0x80);

    // sandwich registers
    let mut c = Cpu::new();
    c.a  = 0x01;
    c.f  = 0x02;
    c.b  = 0x03;
    c.c  = 0x04;
    c.d  = 0x05;
    c.e  = 0x06;
    c.h  = 0x07;
    c.l  = 0x08;
    c.sp = 0xfafa;

    assert_eq!(c.srr(&SR::AF), (0x01 << 8) | 0x02);
    c.srw(&SR::AF, 0xdead);
    assert_eq!(c.a, 0xde);
    assert_eq!(c.f, 0xad);

    assert_eq!(c.srr(&SR::BC), (0x03 << 8) | 0x04);
    c.srw(&SR::BC, 0xdead);
    assert_eq!(c.b, 0xde);
    assert_eq!(c.c, 0xad);

    assert_eq!(c.srr(&SR::DE), (0x05 << 8) | 0x06);
    c.srw(&SR::DE, 0xdead);
    assert_eq!(c.d, 0xde);
    assert_eq!(c.e, 0xad);

    assert_eq!(c.srr(&SR::HL), (0x07 << 8) | 0x08);
    c.srw(&SR::HL, 0xdead);
    assert_eq!(c.d, 0xde);
    assert_eq!(c.e, 0xad);

    assert_eq!(c.srr(&SR::SP), 0xfafa);
    c.srw(&SR::SP, 0xafaf);
    assert_eq!(c.sp, 0xafaf);
}

#[test]
fn test_reset() {
    let mut c = Cpu::new();
    c.a = 0xda;
    c.f = 0xad;
    c.b = 0xbe;
    c.c = 0xef;
    c.d = 0xde;
    c.e = 0xed;
    c.ix = 0xdead;
    c.iy = 0xbeef;
    c.pc = 0x7fff;
    c.sp = 0x8f8f;
    c.ram[0x7f7f] = 0x7f;

    c.reset();

    assert_eq!(c.a, 0x00);
    assert_eq!(c.f, 0x00);
    assert_eq!(c.b, 0x00);
    assert_eq!(c.c, 0x00);
    assert_eq!(c.d, 0x00);
    assert_eq!(c.e, 0x00);
    assert_eq!(c.ix, 0x0000);
    assert_eq!(c.iy, 0x0000);
    assert_eq!(c.pc, 0x0000);
    assert_eq!(c.sp, 0x0000);
    assert_eq!(&c.ram[..], [0; RAM_SIZE]);
}

#[test]
fn test_flags() {
    let mut c = Cpu::new();

    // flag writes
    c.fw(&Flag::C, true);
    assert_eq!(c.f & 0b00000001, 0b00000001);
    c.fw(&Flag::N, true);
    assert_eq!(c.f & 0b00000010, 0b00000010);
    c.fw(&Flag::PV, true);
    assert_eq!(c.f & 0b00000100, 0b00000100);
    c.fw(&Flag::H, true);
    assert_eq!(c.f & 0b00010000, 0b00010000);
    c.fw(&Flag::Z, true);
    assert_eq!(c.f & 0b01000000, 0b01000000);
    c.fw(&Flag::S, true);
    assert_eq!(c.f & 0b10000000, 0b10000000);

    // flag reads
    assert_eq!(c.fr(&Flag::C), true); // C
    c.fw(&Flag::C, false);
    assert_eq!(c.fr(&Flag::C), false);

    assert_eq!(c.fr(&Flag::N), true); // N
    c.fw(&Flag::N, false);
    assert_eq!(c.fr(&Flag::N), false); 

    assert_eq!(c.fr(&Flag::PV), true); // PV
    c.fw(&Flag::PV, false);
    assert_eq!(c.fr(&Flag::PV), false);

    assert_eq!(c.fr(&Flag::H), true); // H
    c.fw(&Flag::H, false);
    assert_eq!(c.fr(&Flag::H), false);

    assert_eq!(c.fr(&Flag::Z), true); // Z
    c.fw(&Flag::Z, false);
    assert_eq!(c.fr(&Flag::Z), false);

    assert_eq!(c.fr(&Flag::S), true); // S
    c.fw(&Flag::S, false);
    assert_eq!(c.fr(&Flag::S), false);
}

#[test]
fn test_decode_0x0() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x00)?;
    assert_eq!(i, Instr::NOP);
    let i = c.decode(0x01)?;
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::BC), Arg16::U16));
    let i = c.decode(0x02)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::BC)), Arg8::Reg(R::A)));
    let i = c.decode(0x03)?;
    assert_eq!(i, Instr::INC16(Arg16::Reg(SR::BC)));
    let i = c.decode(0x04)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::B)));
    let i = c.decode(0x05)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::B)));
    let i = c.decode(0x06)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::U8));
    let i = c.decode(0x07)?;
    assert_eq!(i, Instr::RLCA);
    let i = c.decode(0x08)?;
    assert_eq!(i, Instr::EX(Arg16::Reg(SR::AF), Arg16::Reg(SR::AF)));
    let i = c.decode(0x09)?;
    assert_eq!(i, Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::BC)));
    let i = c.decode(0x0a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::BC))));
    let i = c.decode(0x0b)?;
    assert_eq!(i, Instr::DEC16(Arg16::Reg(SR::BC)));
    let i = c.decode(0x0c)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::C)));
    let i = c.decode(0x0d)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::C)));
    let i = c.decode(0x0e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::U8));
    let i = c.decode(0x0f)?;
    assert_eq!(i, Instr::RRCA);
    
    Ok(())
}

#[test]
fn test_decode_0x1() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x10)?;
    assert_eq!(i, Instr::DJNZ(Arg8::U8));
    let i = c.decode(0x11)?;
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::DE), Arg16::U16));
    let i = c.decode(0x12)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::DE)), Arg8::Reg(R::A)));
    let i = c.decode(0x13)?;
    assert_eq!(i, Instr::INC16(Arg16::Reg(SR::DE)));
    let i = c.decode(0x14)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::D)));
    let i = c.decode(0x15)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::D)));
    let i = c.decode(0x16)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::U8));
    let i = c.decode(0x17)?;
    assert_eq!(i, Instr::RLA);
    let i = c.decode(0x18)?;
    assert_eq!(i, Instr::JR(ArgF::True, Arg8::U8));
    let i = c.decode(0x19)?;
    assert_eq!(i, Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::DE)));
    let i = c.decode(0x1a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::DE))));
    let i = c.decode(0x1b)?;
    assert_eq!(i, Instr::DEC16(Arg16::Reg(SR::DE)));
    let i = c.decode(0x1c)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::E)));
    let i = c.decode(0x1d)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::E)));
    let i = c.decode(0x1e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::U8));
    let i = c.decode(0x1f)?;
    assert_eq!(i, Instr::RRA);

    Ok(())
}

#[test]
fn test_decode_0x2() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x20)?;
    assert_eq!(i, Instr::JR(ArgF::NF(Flag::Z), Arg8::U8));
    let i = c.decode(0x21)?;
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::HL), Arg16::U16));
    let i = c.decode(0x22)?;
    assert_eq!(i, Instr::LD16(Arg16::Mem(MemAddr::Imm), Arg16::Reg(SR::HL)));
    let i = c.decode(0x23)?;
    assert_eq!(i, Instr::INC16(Arg16::Reg(SR::HL)));
    let i = c.decode(0x24)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::H)));
    let i = c.decode(0x25)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::H)));
    let i = c.decode(0x26)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::U8));
    let i = c.decode(0x27)?;
    assert_eq!(i, Instr::DAA);
    let i = c.decode(0x28)?;
    assert_eq!(i, Instr::JR(ArgF::F(Flag::Z), Arg8::U8));
    let i = c.decode(0x29)?;
    assert_eq!(i, Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::HL)));
    let i = c.decode(0x2a)?;
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::HL), Arg16::Mem(MemAddr::Imm)));
    let i = c.decode(0x2b)?;
    assert_eq!(i, Instr::DEC16(Arg16::Reg(SR::HL)));
    let i = c.decode(0x2c)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::L)));
    let i = c.decode(0x2d)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::L)));
    let i = c.decode(0x2e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::U8));
    let i = c.decode(0x2f)?;
    assert_eq!(i, Instr::CPL);

    Ok(())
}

#[test]
fn test_decode_0x3() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x30)?;
    assert_eq!(i, Instr::JR(ArgF::NF(Flag::C), Arg8::U8));
    let i = c.decode(0x31)?;
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::SP), Arg16::U16));
    let i = c.decode(0x32)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Imm), Arg8::Reg(R::A)));
    let i = c.decode(0x33)?;
    assert_eq!(i, Instr::INC16(Arg16::Reg(SR::SP)));
    let i = c.decode(0x34)?;
    assert_eq!(i, Instr::INC8(Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x35)?;
    assert_eq!(i, Instr::DEC8(Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x36)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::U8));
    let i = c.decode(0x37)?;
    assert_eq!(i, Instr::SCF);
    let i = c.decode(0x38)?;
    assert_eq!(i, Instr::JR(ArgF::F(Flag::C), Arg8::U8));
    let i = c.decode(0x39)?;
    assert_eq!(i, Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::SP)));
    let i = c.decode(0x3a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Imm)));
    let i = c.decode(0x3b)?;
    assert_eq!(i, Instr::DEC16(Arg16::Reg(SR::SP)));
    let i = c.decode(0x3c)?;
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::A)));
    let i = c.decode(0x3d)?;
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::A)));
    let i = c.decode(0x3e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::U8));
    let i = c.decode(0x3f)?;
    assert_eq!(i, Instr::CCF);

    Ok(())
}

#[test]
fn test_decode_0x4() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x40)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::B)));
    let i = c.decode(0x41)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::C)));
    let i = c.decode(0x42)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::D)));
    let i = c.decode(0x43)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::E)));
    let i = c.decode(0x44)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::H)));
    let i = c.decode(0x45)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::L)));
    let i = c.decode(0x46)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x47)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::Reg(R::A)));
    let i = c.decode(0x48)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::B)));
    let i = c.decode(0x49)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::C)));
    let i = c.decode(0x4a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::D)));
    let i = c.decode(0x4b)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::E)));
    let i = c.decode(0x4c)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::H)));
    let i = c.decode(0x4d)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::L)));
    let i = c.decode(0x4e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x4f)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_decode_0x5() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x50)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::B)));
    let i = c.decode(0x51)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::C)));
    let i = c.decode(0x52)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::D)));
    let i = c.decode(0x53)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::E)));
    let i = c.decode(0x54)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::H)));
    let i = c.decode(0x55)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::L)));
    let i = c.decode(0x56)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x57)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::D), Arg8::Reg(R::A)));
    let i = c.decode(0x58)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::B)));
    let i = c.decode(0x59)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::C)));
    let i = c.decode(0x5a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::D)));
    let i = c.decode(0x5b)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::E)));
    let i = c.decode(0x5c)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::H)));
    let i = c.decode(0x5d)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::L)));
    let i = c.decode(0x5e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x5f)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::E), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_decode_0x6() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x60)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::B)));
    let i = c.decode(0x61)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::C)));
    let i = c.decode(0x62)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::D)));
    let i = c.decode(0x63)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::E)));
    let i = c.decode(0x64)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::H)));
    let i = c.decode(0x65)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::L)));
    let i = c.decode(0x66)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x67)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::H), Arg8::Reg(R::A)));
    let i = c.decode(0x68)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::B)));
    let i = c.decode(0x69)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::C)));
    let i = c.decode(0x6a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::D)));
    let i = c.decode(0x6b)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::E)));
    let i = c.decode(0x6c)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::H)));
    let i = c.decode(0x6d)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::L)));
    let i = c.decode(0x6e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x6f)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::L), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_decode_0x7() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x70)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::B)));
    let i = c.decode(0x71)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::C)));
    let i = c.decode(0x72)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::D)));
    let i = c.decode(0x73)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::E)));
    let i = c.decode(0x74)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::H)));
    let i = c.decode(0x75)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::L)));
    let i = c.decode(0x76)?;
    assert_eq!(i, Instr::HALT);
    let i = c.decode(0x77)?;
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::HL)), Arg8::Reg(R::A)));
    let i = c.decode(0x78)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::B)));
    let i = c.decode(0x79)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::C)));
    let i = c.decode(0x7a)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::D)));
    let i = c.decode(0x7b)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::E)));
    let i = c.decode(0x7c)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::H)));
    let i = c.decode(0x7d)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::L)));
    let i = c.decode(0x7e)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x7f)?;
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_decode_0x8() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x80)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::B)));
    let i = c.decode(0x81)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::C)));
    let i = c.decode(0x82)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::D)));
    let i = c.decode(0x83)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::E)));
    let i = c.decode(0x84)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::H)));
    let i = c.decode(0x85)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::L)));
    let i = c.decode(0x86)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x87)?;
    assert_eq!(i, Instr::ADD8(Arg8::Reg(R::A), Arg8::Reg(R::A)));
    let i = c.decode(0x88)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::B)));
    let i = c.decode(0x89)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::C)));
    let i = c.decode(0x8a)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::D)));
    let i = c.decode(0x8b)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::E)));
    let i = c.decode(0x8c)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::H)));
    let i = c.decode(0x8d)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::L)));
    let i = c.decode(0x8e)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x8f)?;
    assert_eq!(i, Instr::ADC8(Arg8::Reg(R::A), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_decode_0x9() -> Result<(), Error> {
    let c = Cpu::new();

    let i = c.decode(0x90)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::B)));
    let i = c.decode(0x91)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::C)));
    let i = c.decode(0x92)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::D)));
    let i = c.decode(0x93)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::E)));
    let i = c.decode(0x94)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::H)));
    let i = c.decode(0x95)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::L)));
    let i = c.decode(0x96)?;
    assert_eq!(i, Instr::SUB8(Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x97)?;
    assert_eq!(i, Instr::SUB8(Arg8::Reg(R::A)));
    let i = c.decode(0x98)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::B)));
    let i = c.decode(0x99)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::C)));
    let i = c.decode(0x9a)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::D)));
    let i = c.decode(0x9b)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::E)));
    let i = c.decode(0x9c)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::H)));
    let i = c.decode(0x9d)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::L)));
    let i = c.decode(0x9e)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::HL))));
    let i = c.decode(0x9f)?;
    assert_eq!(i, Instr::SBC8(Arg8::Reg(R::A), Arg8::Reg(R::A)));

    Ok(())
}

#[test]
fn test_u8_arg() -> Result<(), Error> {
    let mut c = Cpu::new();

    // op X, R
    c.rw(&R::A, 0xfe);
    let (v, pc) = c.u8_arg(&Arg8::Reg(R::A))?;
    assert_eq!(v, 0xfe);
    assert_eq!(pc, PC::I);

    // op X, *
    insert_imm8(&mut c, 0xef);
    let (v, pc) = c.u8_arg(&Arg8::U8)?;
    assert_eq!(v, 0xef);
    assert_eq!(pc, PC::Im8);

    // op X, (**)
    insert_imm16(&mut c, 0xfeed);
    c.write(0xfeed, 0xaf)?;
    let (v, pc) = c.u8_arg(&Arg8::Mem(MemAddr::Imm))?;
    assert_eq!(v, 0xaf);
    assert_eq!(pc, PC::Im16);

    // op X, (SR)
    c.srw(&SR::HL, 0xdeef);
    c.write(0xdeef, 0xfa)?;
    let (v, pc) = c.u8_arg(&Arg8::Mem(MemAddr::Reg(SR::HL)))?;
    assert_eq!(v, 0xfa);
    assert_eq!(pc, PC::I);

    Ok(())
}

#[test]
fn test_u16_arg() -> Result<(), Error> {
    let mut c = Cpu::new();

    // op XX, HL
    c.srw(&SR::HL, 0xfeed);
    let (v, pc) = c.u16_arg(&Arg16::Reg(SR::HL))?;
    assert_eq!(v, 0xfeed);
    assert_eq!(pc, PC::I);

    // op XX, **
    insert_imm16(&mut c, 0xffee);
    let (v, pc) = c.u16_arg(&Arg16::U16)?;
    assert_eq!(v, 0xffee);
    assert_eq!(pc, PC::Im16);

    // op XX, [**] (mismatch length)
    let e = c.u16_arg(&Arg16::Mem(MemAddr::Imm)).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_nop() -> Result<(), Error> {
    let mut c = Cpu::new();
    c.execute(&c.decode(0x00)?)?;
    assert_eq!(c.pc, 0x0001);
    Ok(())
}

#[test]
fn test_execute_ld8() -> Result<(), Error> {
    // ld (SR), R
    let mut c = Cpu::new();
    c.rw(&R::A, 0xfe);
    c.srw(&SR::BC, 0xff00);
    c.execute(&c.decode(0x02)?)?;
    assert_eq!(c.read(0xff00)?, 0xfe);

    // ld R, *
    let mut c = Cpu::new();
    insert_imm8(&mut c, 0xfe);
    c.execute(&c.decode(0x06)?)?;
    assert_eq!(c.rr(&R::B), 0xfe);

    // ld R, (SR)
    let mut c = Cpu::new();
    c.srw(&SR::BC, 0xff00);
    c.write(0xff00, 0xfe)?;
    c.execute(&c.decode(0x0a)?)?;
    assert_eq!(c.rr(&R::A), 0xfe);

    // ld *, X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::LD8(Arg8::U8, Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_ld16() -> Result<(), Error> {
    // ld SR, **
    let mut c = Cpu::new();
    insert_imm16(&mut c, 0xdead);
    c.execute(&c.decode(0x01)?)?;
    assert_eq!(c.pc, 0x0003);
    assert_eq!(c.srr(&SR::BC), 0xdead);

    // ld **, XX
    let mut c = Cpu::new();
    let e = c.execute(&Instr::LD16(Arg16::U16, Arg16::Reg(SR::HL))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // ld [**], XX
    let mut c = Cpu::new();
    let e = c.execute(&Instr::LD16(Arg16::Mem(MemAddr::Imm), Arg16::Reg(SR::HL))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_inc8() -> Result<(), Error> {
    // inc R
    let mut c = Cpu::new();
    c.rw(&R::B, 0xff);
    c.execute(&c.decode(0x04)?)?;
    assert_eq!(c.rr(&R::B), 0x00);

    // inc *
    let mut c = Cpu::new();
    let e = c.execute(&Instr::INC8(Arg8::U8)).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // inc [**]
    let mut c = Cpu::new();
    let e = c.execute(&Instr::INC8(Arg8::Mem(MemAddr::Imm))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // inc [SR]
    let mut c = Cpu::new();
    c.srw(&SR::HL, 0xfefe);
    c.write(0xfefe, 0xaf)?;
    c.execute(&c.decode(0x34)?)?;
    assert_eq!(c.read(0xfefe)?, 0xaf + 1);

    Ok(())
}

#[test]
fn test_execute_inc16() -> Result<(), Error> {
    // inc SR
    let mut c = Cpu::new();
    c.srw(&SR::BC, 0xffff);
    c.execute(&c.decode(0x03)?)?;
    assert_eq!(c.srr(&SR::BC), 0x0000);

    // inc **
    let mut c = Cpu::new();
    let e = c.execute(&Instr::INC16(Arg16::U16)).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // inc [**]
    let mut c = Cpu::new();
    let e = c.execute(&Instr::INC16(Arg16::Mem(MemAddr::Imm))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_adc8() -> Result<(), Error> {
    // adc R, R
    let mut c = Cpu::new();
    c.rw(&R::A, 0x02);
    c.fw(&Flag::C, true);
    c.execute(&c.decode(0x8f)?)?;
    assert_eq!(c.rr(&R::A), 0x05);
    c.rw(&R::A, 0x80);
    c.fw(&Flag::C, true);
    c.execute(&c.decode(0x8f)?)?;
    assert_eq!(c.rr(&R::A), 0x01);

    // adc *, X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADC8(Arg8::U8, Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // adc [**], X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADC8(Arg8::Mem(MemAddr::Imm), Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_add8() -> Result<(), Error> {
    // add R, R
    let mut c = Cpu::new();
    c.rw(&R::A, 0x02);
    c.execute(&c.decode(0x87)?)?;
    assert_eq!(c.rr(&R::A), 0x04);
    c.rw(&R::A, 0x80);
    c.execute(&c.decode(0x87)?)?;
    assert_eq!(c.rr(&R::A), 0x00);

    // add *, X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADD8(Arg8::U8, Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // add [**], X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADD8(Arg8::Mem(MemAddr::Imm), Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_add16() -> Result<(), Error> {
    // add SR, SR
    let mut c = Cpu::new();
    c.srw(&SR::BC, 0x000f);
    c.srw(&SR::HL, 0x00f0);
    c.execute(&c.decode(0x09)?)?;
    assert_eq!(c.srr(&SR::HL), 0x00ff);

    // add **, XX
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADD16(Arg16::U16, Arg16::Reg(SR::HL))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // add [**], XX
    let mut c = Cpu::new();
    let e = c.execute(&Instr::ADD16(Arg16::Mem(MemAddr::Imm), Arg16::Reg(SR::HL))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_sub8() -> Result<(), Error> {
    // sub R
    let mut c = Cpu::new();
    c.rw(&R::A, 0xff);
    c.rw(&R::B, 0x0f);
    c.execute(&c.decode(0x90)?)?;
    assert_eq!(c.rr(&R::A), 0xf0);
    Ok(())
}

#[test]
fn test_execute_sbc8() -> Result<(), Error> {
    // sbc R, R
    let mut c = Cpu::new();
    c.rw(&R::A, 0xff);
    c.rw(&R::B, 0x0f);
    c.fw(&Flag::C, true);
    c.execute(&c.decode(0x98)?)?;
    assert_eq!(c.rr(&R::A), 0xef);
    c.rw(&R::A, 0x02);
    c.fw(&Flag::C, true);
    c.execute(&c.decode(0x9f)?)?;
    assert_eq!(c.rr(&R::A), 0xff);

    // sbc *, X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::SBC8(Arg8::U8, Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // adc [**], X
    let mut c = Cpu::new();
    let e = c.execute(&Instr::SBC8(Arg8::Mem(MemAddr::Imm), Arg8::Reg(R::A))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_dec8() -> Result<(), Error> {
    // dec R
    let mut c = Cpu::new();
    c.rw(&R::B, 0x00);
    c.execute(&c.decode(0x05)?)?;
    assert_eq!(c.rr(&R::B), 0xff);

    // dec *
    let mut c = Cpu::new();
    let e = c.execute(&Instr::DEC8(Arg8::U8)).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // dec [**]
    let mut c = Cpu::new();
    let e = c.execute(&Instr::DEC8(Arg8::Mem(MemAddr::Imm))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // dec [SR]
    let mut c = Cpu::new();
    c.srw(&SR::HL, 0xfefe);
    c.write(0xfefe, 0xaf)?;
    c.execute(&c.decode(0x35)?)?;
    assert_eq!(c.read(0xfefe)?, 0xaf - 1);

    Ok(())
}

#[test]
fn test_execute_dec16() -> Result<(), Error> {
    // dec SR
    let mut c = Cpu::new();
    c.srw(&SR::BC, 0x0000);
    c.execute(&c.decode(0x0b)?)?;
    assert_eq!(c.srr(&SR::BC), 0xffff);

    // dec **
    let mut c = Cpu::new();
    let e = c.execute(&Instr::DEC16(Arg16::U16)).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    // dec [**]
    let mut c = Cpu::new();
    let e = c.execute(&Instr::DEC16(Arg16::Mem(MemAddr::Imm))).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));

    Ok(())
}

#[test]
fn test_execute_rlca() -> Result<(), Error> {
    // rlca
    let mut c = Cpu::new();
    c.rw(&R::A, 0b0000_0001);
    c.execute(&c.decode(0x07)?)?;
    assert_eq!(c.rr(&R::A), 0b0000_0010);
    c.rw(&R::A, 0b1000_1000);
    c.execute(&c.decode(0x07)?)?;
    assert_eq!(c.rr(&R::A), 0b0001_0001);

    Ok(())
}

#[test]
fn test_execute_rrca() -> Result<(), Error> {
    // rrca
    let mut c = Cpu::new();
    c.rw(&R::A, 0b0000_0010);
    c.execute(&c.decode(0x0f)?)?;
    assert_eq!(c.rr(&R::A), 0b0000_0001);
    c.rw(&R::A, 0b0001_0001);
    c.execute(&c.decode(0x0f)?)?;
    assert_eq!(c.rr(&R::A), 0b1000_1000);

    Ok(())
}

#[test]
fn test_execute_rla() -> Result<(), Error> {
    // rla
    let mut c = Cpu::new();
    c.fw(&Flag::C, true);
    c.rw(&R::A, 0b0100_0000);
    c.execute(&c.decode(0x17)?)?;
    assert_eq!(c.rr(&R::A), 0b1000_0001);

    c.fw(&Flag::C, false);
    c.rw(&R::A, 0b1000_0000);
    c.execute(&c.decode(0x17)?)?;
    assert_eq!(c.rr(&R::A), 0b0000_0000);

    Ok(())
}

#[test]
fn test_execute_rra() -> Result<(), Error> {
    // rra
    let mut c = Cpu::new();
    c.fw(&Flag::C, true);
    c.rw(&R::A, 0b0000_0010);
    c.execute(&c.decode(0x1f)?)?;
    assert_eq!(c.rr(&R::A), 0b1000_0001);

    c.fw(&Flag::C, false);
    c.rw(&R::A, 0b0000_0001);
    c.execute(&c.decode(0x1f)?)?;
    assert_eq!(c.rr(&R::A), 0b0000_0000);
    Ok(())
}

#[test]
fn test_execute_djnz() -> Result<(), Error> {
    // djnz *
    let mut c = Cpu::new();
    c.fw(&Flag::Z, false);
    insert_imm8(&mut c, (50 as i8) as u8);
    c.execute(&c.decode(0x10)?)?;
    assert_eq!(c.pc as u16, 50);

    let mut c = Cpu::new();
    c.fw(&Flag::Z, true);
    insert_imm8(&mut c, (-1 as i8) as u8);
    c.execute(&c.decode(0x10)?)?;
    assert_eq!(c.pc as u16, 2);

    Ok(())
}

#[test]
fn test_execute_jr() -> Result<(), Error> {
    // jr *
    let mut c = Cpu::new();
    // jump forward
    insert_imm8(&mut c, (50 as i8) as u8);
    c.execute(&c.decode(0x18)?)?;
    assert_eq!(c.pc, 50);
    // jump backward
    insert_imm8(&mut c, (-5 as i8) as u8);
    c.execute(&c.decode(0x18)?)?;
    assert_eq!(c.pc, 45);

    // jr F, *
    let mut c = Cpu::new();
    c.fw(&Flag::Z, true);
    insert_imm8(&mut c, (50 as i8) as u8);
    c.execute(&c.decode(0x28)?)?;
    assert_eq!(c.pc, 50);
    c.fw(&Flag::Z, false);
    insert_imm8(&mut c, (-1 as i8) as u8);
    c.execute(&c.decode(0x28)?)?;
    assert_eq!(c.pc, 52);

    // jr NF, *
    let mut c = Cpu::new();
    c.fw(&Flag::Z, false);
    insert_imm8(&mut c, (50 as i8) as u8);
    c.execute(&c.decode(0x20)?)?;
    assert_eq!(c.pc, 50);
    c.fw(&Flag::Z, true);
    insert_imm8(&mut c, (-5 as i8) as u8);
    c.execute(&c.decode(0x20)?)?;
    assert_eq!(c.pc, 52);

    Ok(())
}

#[test]
fn test_execute_ex() -> Result<(), Error> {
    // TODO: shadow registers
    Ok(())
}

#[test]
fn test_execute_daa() -> Result<(), Error> {
    // TODO: BCD
    Ok(())
}

#[test]
fn test_execute_cpl() -> Result<(), Error> {
    // cpl
    let mut c = Cpu::new();
    c.rw(&R::A, 0b1010_0101);
    c.execute(&c.decode(0x2f)?)?;
    assert_eq!(c.rr(&R::A), 0b0101_1010);

    Ok(())
}

#[test]
fn test_execute_scf() -> Result<(), Error> {
    // scf
    let mut c = Cpu::new();
    assert_eq!(c.fr(&Flag::C), false);
    c.execute(&c.decode(0x37)?)?;
    assert_eq!(c.fr(&Flag::C), true);

    Ok(())
}

#[test]
fn test_execute_ccf() -> Result<(), Error> {
    // scf
    let mut c = Cpu::new();
    c.fw(&Flag::C, true);
    c.execute(&c.decode(0x3f)?)?;
    assert_eq!(c.fr(&Flag::C), false);

    Ok(())
}
