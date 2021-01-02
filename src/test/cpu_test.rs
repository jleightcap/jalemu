use super::*;

#[test]
fn test_read() {
    // valid reads
    let mut c = Cpu::new();
    c.rom[0x0000] = 0xde; // NOTE: don't actually write to memory like this,
    c.rom[0x0001] = 0xad; // just an independent test from self.write()
    assert_eq!(c.read(0x0000).unwrap(), 0xde);
    assert_eq!(c.read(0x0001).unwrap(), 0xad);
    c.ram[0x0000] = 0xbe; // similarly, ram shouldn't be written like this
    c.ram[0x0001] = 0xef; // ram[0x0000] mapped to 0x8000
    c.ram[0x7fff] = 0xff;
    assert_eq!(c.read(0x8000).unwrap(), 0xbe);
    assert_eq!(c.read(0x8001).unwrap(), 0xef);
    assert_eq!(c.read(0xffff).unwrap(), 0xff);

    // invalid reads
    let c = Cpu::new();
    let e = c.read(0x10000).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));
}

#[test]
fn test_write() {
    // valid writes
    let mut c = Cpu::new();
    c.write(0x8000, 0xbe).unwrap();
    c.write(0x8001, 0xef).unwrap();
    assert_eq!(c.ram[0x0000], 0xbe); // NOTE: don't actually read from memory like this,
    assert_eq!(c.ram[0x0001], 0xef); // just an independent test from self.read()
    c.write(0x0000, 0xde).unwrap();
    c.write(0x0001, 0xad).unwrap();
    assert_eq!(c.rom[0x0000], 0x00); // writes to ROM have no affect
    assert_eq!(c.rom[0x0001], 0x00);

    // invalid writes
    let mut c = Cpu::new();
    let e = c.write(0x10000, 0xff).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));
}

#[test]
fn test_fetch() {
    // valid fetch
    let mut c = Cpu::new();
    c.rom[0x0000] = 0xde;
    c.rom[0x0001] = 0xad;
    assert_eq!(c.fetch().unwrap(), 0xde);
    c.pc += 1;
    assert_eq!(c.fetch().unwrap(), 0xad);

    // invalid fetch
    c.pc = 0x10000;
    let e = c.fetch().map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));
}

#[test]
fn test_sandwich() {
    let mut c = Cpu::new();
    c.a = 0x01;
    c.f = 0x02;
    c.b = 0x03;
    c.c = 0x04;
    c.d = 0x05;
    c.e = 0x06;
    c.h = 0x07;
    c.l = 0x08;

    assert_eq!(c.srr(SR::AF), (0x01 << 8) | 0x02);
    c.srw(SR::AF, 0xdead);
    assert_eq!(c.a, 0xde);
    assert_eq!(c.f, 0xad);

    assert_eq!(c.srr(SR::BC), (0x03 << 8) | 0x04);
    c.srw(SR::BC, 0xdead);
    assert_eq!(c.b, 0xde);
    assert_eq!(c.c, 0xad);

    assert_eq!(c.srr(SR::DE), (0x05 << 8) | 0x06);
    c.srw(SR::DE, 0xdead);
    assert_eq!(c.d, 0xde);
    assert_eq!(c.e, 0xad);

    assert_eq!(c.srr(SR::HL), (0x07 << 8) | 0x08);
    c.srw(SR::HL, 0xdead);
    assert_eq!(c.d, 0xde);
    assert_eq!(c.e, 0xad);
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
    c.fw(Flag::C, true);
    assert_eq!(c.f & 0b00000001, 0b00000001);
    c.fw(Flag::N, true);
    assert_eq!(c.f & 0b00000010, 0b00000010);
    c.fw(Flag::PV, true);
    assert_eq!(c.f & 0b00000100, 0b00000100);
    c.fw(Flag::H, true);
    assert_eq!(c.f & 0b00010000, 0b00010000);
    c.fw(Flag::Z, true);
    assert_eq!(c.f & 0b01000000, 0b01000000);
    c.fw(Flag::S, true);
    assert_eq!(c.f & 0b10000000, 0b10000000);

    // flag reads
    assert_eq!(c.fr(Flag::C), true); // C
    c.fw(Flag::C, false);
    assert_eq!(c.fr(Flag::C), false);

    assert_eq!(c.fr(Flag::N), true); // N
    c.fw(Flag::N, false);
    assert_eq!(c.fr(Flag::N), false); 

    assert_eq!(c.fr(Flag::PV), true); // PV
    c.fw(Flag::PV, false);
    assert_eq!(c.fr(Flag::PV), false);

    assert_eq!(c.fr(Flag::H), true); // H
    c.fw(Flag::H, false);
    assert_eq!(c.fr(Flag::H), false);

    assert_eq!(c.fr(Flag::Z), true); // Z
    c.fw(Flag::Z, false);
    assert_eq!(c.fr(Flag::Z), false);

    assert_eq!(c.fr(Flag::S), true); // S
    c.fw(Flag::S, false);
    assert_eq!(c.fr(Flag::S), false);
}

#[test]
fn test_decode_0x0() {
    let c = Cpu::new();

    let i = c.decode(0x00).unwrap();
    assert_eq!(i, Instr::NOP);
    let i = c.decode(0x01).unwrap();
    assert_eq!(i, Instr::LD16(Arg16::Reg(SR::BC), Arg16::U16));
    let i = c.decode(0x02).unwrap();
    assert_eq!(i, Instr::LD8(Arg8::Mem(MemAddr::Reg(SR::BC)), Arg8::Reg(R::A)));
    let i = c.decode(0x03).unwrap();
    assert_eq!(i, Instr::INC16(Arg16::Reg(SR::BC)));
    let i = c.decode(0x04).unwrap();
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::B)));
    let i = c.decode(0x05).unwrap();
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::B)));
    let i = c.decode(0x06).unwrap();
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::B), Arg8::U8));
    let i = c.decode(0x07).unwrap();
    assert_eq!(i, Instr::RLCA);
    // TODO: ex af, af'
    let i = c.decode(0x09).unwrap();
    assert_eq!(i, Instr::ADD16(Arg16::Reg(SR::HL), Arg16::Reg(SR::BC)));
    let i = c.decode(0x0a).unwrap();
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::A), Arg8::Mem(MemAddr::Reg(SR::BC))));
    let i = c.decode(0x0b).unwrap();
    assert_eq!(i, Instr::DEC16(Arg16::Reg(SR::BC)));
    let i = c.decode(0x0c).unwrap();
    assert_eq!(i, Instr::INC8(Arg8::Reg(R::C)));
    let i = c.decode(0x0d).unwrap();
    assert_eq!(i, Instr::DEC8(Arg8::Reg(R::C)));
    let i = c.decode(0x0e).unwrap();
    assert_eq!(i, Instr::LD8(Arg8::Reg(R::C), Arg8::U8));
    let i = c.decode(0x0f).unwrap();
    assert_eq!(i, Instr::RRCA);
}
