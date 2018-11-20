use std::io::Read;

pub struct Memory {
    bios:         Vec<u8>,  // 16 KB;  00000000-00003FFF
    wram_onboard: Vec<u8>,  // 256 KB; 02000000-0203FFFF; 2 wait
    wram_inchip:  Vec<u8>,  // 32 KB;  03000000-03007FFF
    io:           Vec<u8>,  // 1 KB;   04000000-040003FE (1022)
    palette:      Vec<u8>,  // 1 KB;   05000000-050003FF
    vram:         Vec<u8>,  // 96 KB;  06000000-06017FFF
    obj:          Vec<u8>,  // 1 KB;   07000000-070003FF
    rom:          Vec<u8>,  // 32 MB;  08000000-09FFFFFF; wait state 0
                            //         0A000000-0BFFFFFF; wait state 1
                            //         0C000000-0DFFFFFF; wait state 2
    sram:         Vec<u8>,  // 64 KB;  0E000000-0E00FFFF; 8 bit bus
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            bios         : vec![0; 0x4000],
            wram_onboard : vec![0; 0x4_0000],
            wram_inchip  : vec![0; 0x8000],
            io           : vec![0; 0x0400],
            palette      : vec![0; 0x0400],
            vram         : vec![0; 0x1_8000],
            obj          : vec![0; 0x0400],
            rom          : vec![0; 0x0200_0000],
            sram         : vec![0; 0x1_0000],
        }
    }

    pub fn add_bios(&mut self, bios: Vec<u8>) {
        for (i, byte) in bios.iter().enumerate() {
            self.bios[i] = *byte;
        }
    }

    pub fn add_rom(&mut self, rom: Vec<u8>) {
        for (i, byte) in rom.iter().enumerate() {
            self.rom[i] = *byte;
        }
    }

    fn read(&self, start: usize, length: usize) -> Vec<u8> {
        let msb = start >> 24 as u8;
        let (section, offset) = match msb {
            0x00 => (&self.bios         , start - 0x00000000),
            0x02 => (&self.wram_onboard , start - 0x02000000),
            0x03 => (&self.wram_inchip  , start - 0x03000000),
            0x04 => (&self.io           , start - 0x04000000),
            0x05 => (&self.palette      , start - 0x05000000),
            0x06 => (&self.vram         , start - 0x06000000),
            0x07 => (&self.obj          , start - 0x07000000),
            0x08 => (&self.rom          , start - 0x08000000),
            0x0A => (&self.rom          , start - 0x0A000000),
            0x0C => (&self.rom          , start - 0x0C000000),
            0x0E => (&self.sram         , start - 0x0E000000),
            _    => panic!("OOB read: {:x}",           start),
        };

        section[offset..length + offset].to_vec()
    }

    pub fn read_byte(&self, start: usize) -> u8 {
        self.read(start, 1)[0]
    }

    pub fn read_halfword(&self, start: usize) -> u16 {
        let bytes = self.read(start, 2);
        (bytes[0] as u16 + ((bytes[1] as u16) << 8))
    }

    pub fn read_word(&self, start: usize) -> u32 {
        let bytes = self.read(start, 4);
        (bytes[0] as u32 + ((bytes[1] as u32) << 8) + ((bytes[2] as u32) << 16) + ((bytes[3] as u32) << 24))
    }

    fn write(&mut self, values: Vec<u8>, start: usize) {
        let msb = start >> 24 as u8;
        let (section, offset) = match msb {
            0x00 => panic!("can't write to bios: {:x}",  start),
            0x02 => (&mut self.wram_onboard , start - 0x02000000),
            0x03 => (&mut self.wram_inchip  , start - 0x03000000),
            0x04 => (&mut self.io           , start - 0x04000000),
            0x05 => (&mut self.palette      , start - 0x05000000),
            0x06 => (&mut self.vram         , start - 0x06000000),
            0x07 => (&mut self.obj          , start - 0x07000000),
            0x08 => panic!("can't write to rom: {:x}",     start),
            0x0A => panic!("can't write to rom: {:x}",     start),
            0x0C => panic!("can't write to rom: {:x}",     start),
            0x0E => (&mut self.sram         , start - 0x0E000000),
            _    => panic!("OOB write: {:x}",              start),
        };

        let mut pos = offset;
        for val in values {
            section[pos] = val;
            pos += 1;
        }
    }

    pub fn write_byte(&mut self, value: u8, start: usize) {
        self.write(vec![value], start);
    }

    pub fn write_halfword(&mut self, value: u16, start: usize) {
        let bytes: [u8; 2] = value.to_le_bytes();
        self.write(bytes.iter().cloned().collect(), start);
    }

    pub fn write_word(&mut self, value: u32, start: usize) {
        let bytes: [u8; 4] = value.to_le_bytes();
        self.write(bytes.iter().cloned().collect(), start);
    }
}
