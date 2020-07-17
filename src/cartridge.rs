use std::fs::File;
use std::io::Read;

pub struct Cartridge {
    program_rom: [u8; 0x4000]
}

impl Cartridge {

    pub fn program_rom_size(&self) -> usize {
        self.program_rom.len()
    }

    pub fn get_data_at(&self, offset: usize) -> u8 {
        self.program_rom[offset]
    }

    pub fn load_cartridge(filename: &str) -> Cartridge {
        // Only supporting memory mapper 0 right now
        let mut file = File::open(filename).unwrap();

        // Read the 16 byte header
        let mut header = [0u8; 0x10];
        file.read(&mut header).unwrap();

        // Read the PRG-ROM (ignoring other mapper considerations for now... Like a possible 512 byte
        // trainer, etc..)
        let mut prg_rom = [0u8; 0x4000];
        file.read(&mut prg_rom).unwrap();

        Cartridge { program_rom: prg_rom }
    }
}