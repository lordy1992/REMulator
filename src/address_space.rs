use crate::cartridge::Cartridge;
use std::fmt::Formatter;

const RAM_SIZE: usize = 0x0800;

pub struct AddressSpace<'a> {
    cartridge: &'a Cartridge,
    ram: [u8; RAM_SIZE]
}

impl<'a> std::fmt::Debug for AddressSpace<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AddressSpace")
            .field("ram.size", &self.ram.len())
            .finish()
    }
}

impl AddressSpace<'_> {
    pub fn new(cartridge: &Cartridge) -> AddressSpace {
        let ram = [0; 0x0800];

        AddressSpace { ram, cartridge }
    }

    pub fn read_data_u8(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1FFF => {
                // This is the RAM. It is mirrored every 0x0800 bytes, so we must account for that
                // when determining the offset into nes.ram (by taking the address modulo the ram
                // size).
                let offset = (address as usize) % RAM_SIZE;
                self.ram[offset]
            }
            0x2000..=0x4019 => {
                // IO Registers. For now they are unimplemented.
                unimplemented!("IO Registers have not been implemented");
            }
            0x4020..=0x5FFF => {
                // Expansion space - depends on memory mapper. Unused so far.
                unimplemented!("Expansion memory has not been implemented");
            }
            0x6000..=0x7FFF => {
                // The SRAM (for storing save games). Currently unimplemented.
                unimplemented!("Save RAM has not been implemented");
            }
            0x8000..=0xFFFF => {
                // The PRG-ROM. Lower bank is from 0x8000 to 0xC000, and upper bank is from
                // 0xC000 to 0x10000. For NRAM (only implemented memory mapper), upper and lower
                // banks are the same. The offset depends on the length of the ROM in the
                // cartridge.
                let offset = (address as usize) % self.cartridge.program_rom_size();
                self.cartridge.get_data_at(offset)
            }
            _ => {
                unimplemented!("Attempt to read from invalid address ${:04X}", address);
            }
        }
    }

    pub fn read_data_u16(&self, address: u16) -> u16 {
        let low_byte = self.read_data_u8(address);
        let high_byte = self.read_data_u8(address.wrapping_add(1));

        (low_byte as u16) | ((high_byte as u16) << 8)
    }

    pub fn read_u16_zero_page(&self, address: u8) -> u16 {
        let low_byte = self.read_data_u8(address as u16);
        let high_byte = self.read_data_u8(address.wrapping_add(1) as u16);

        (low_byte as u16) | ((high_byte as u16) << 8)
    }

    pub fn write_data(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1FFF => {
                // The RAM. Currently (with the NRAM mapper), this is the only memory space where
                // we allow writes. Mirrored every 0x0800 bytes.
                let offset = (address as usize) % RAM_SIZE;
                self.ram[offset] = value;
            }
            0x2000..=0x4020 => {
                // For now, ignore writes to IO registers
            }
            0x8000..=0xFFFF => {
                // Writes to PRG-ROM are simply ignored
            }
            _ => {
                unimplemented!("Attempted to write to ${:04X?}", address);
            }
        }
    }
}