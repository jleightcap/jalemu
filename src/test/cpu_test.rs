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

    // invalid writes
    let mut c = Cpu::new();
    let e = c.write(0x10000, 0xff).map_err(|e| e.kind());
    assert_eq!(e, Err(ErrorKind::InvalidData));
}

#[test]
fn test_fetch() {
    // valid fetch
    let mut c = Cpu::new();
    c.write(0x0000, 0xde).unwrap();
    c.write(0x0001, 0xad).unwrap();
    c.write(0x0002, 0xbe).unwrap();
    c.write(0x0003, 0xef).unwrap();
    assert_eq!(c.fetch().unwrap(), 0xdead);
    assert_eq!(c.pc, 0x0002);
    assert_eq!(c.fetch().unwrap(), 0xbeef);
    assert_eq!(c.pc, 0x0004);

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
}
