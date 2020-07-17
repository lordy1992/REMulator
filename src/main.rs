mod address_space;
mod cartridge;
mod cpu;

struct NES<'a> {
    cpu: cpu::CPU<'a>,
}

impl<'a> NES<'a> {
    fn boot_nes(address_space: &'a mut address_space::AddressSpace<'a>) -> NES<'a> {
        // Set the initial state of the CPU
        let cpu = cpu::CPU::new(address_space);

        NES { cpu }
    }

    fn run_cycle(&mut self) {
        let mut count = 0;
        loop {
            count += 1;
            // Handle one CPU instr
            self.cpu.execute_next_instruction();

            if count == 10_000 {
                // TODO: Remove when we support turning off/on
                break;
            }
            // Handle 3*CPU_cycles PPU cycles - TODO

            // APU - TODO
        }
    }
}

fn main() {
    let cartridge = cartridge::Cartridge::load_cartridge("nestest.nes");
    let mut address_space = address_space::AddressSpace::new(&cartridge);
    let mut nes = NES::boot_nes(&mut address_space);

    nes.run_cycle();
}