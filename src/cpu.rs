use std::collections::HashMap;

use crate::address_space::AddressSpace;

#[derive(Debug)]
pub struct StatusRegister {
    carry_flag: bool,
    zero_flag: bool,
    interrupt_disable_flag: bool,
    decimal_flag: bool,
    break_flag_1: bool,
    break_flag_2: bool,
    overflow_flag: bool,
    negative_flag: bool
}

impl StatusRegister {
    pub fn new() -> StatusRegister {
        // Initialize to 0xFD, the value of the status register when reset.
        // 0xFD == 1111 1101 ==> All 1 except zero_flag
        StatusRegister { carry_flag: false, zero_flag: false, interrupt_disable_flag: true,
            decimal_flag: false, break_flag_1: false, break_flag_2: true,
            overflow_flag: false, negative_flag: false}
    }

    pub fn as_bytes(&self) -> u8 {
        (self.carry_flag as u8) | ((self.zero_flag as u8) << 1) |
            ((self.interrupt_disable_flag as u8) << 2) | ((self.decimal_flag as u8) << 3) |
                ((self.break_flag_1 as u8) << 4) | ((self.break_flag_2 as u8) << 5) |
            ((self.overflow_flag as u8) << 6) | ((self.negative_flag as u8) << 7)
    }

    pub fn from_bytes(&self, bytes: u8) -> StatusRegister {
        let carry_flag = (bytes & 0x01) != 0;
        let zero_flag = ((bytes >> 1) & 0x01) != 0;
        let interrupt_disable_flag = ((bytes >> 2) & 0x01) != 0;
        let decimal_flag = ((bytes >> 3) & 0x01) != 0;
        let break_flag_1 = ((bytes >> 4) & 0x01) != 0;
        let break_flag_2 = ((bytes >> 5) & 0x01) != 0;
        let overflow_flag = ((bytes >> 6) & 0x01) != 0;
        let negative_flag = ((bytes >> 7) & 0x01) != 0;

        StatusRegister { carry_flag, zero_flag, interrupt_disable_flag, decimal_flag, break_flag_1,
            break_flag_2, overflow_flag, negative_flag }
    }
}

type Cycles = usize;
type Opcode = u8;
type OpcodeToInstrInfo<'a> = HashMap<Opcode, (&'a str, Opcode, Cycles, AddressingMode)>;

pub struct CPU<'a> {
    pc: u16,
    acc: u8,
    x: u8,
    y: u8,
    status: StatusRegister,
    sp: u8,
    opcode_map: OpcodeToInstrInfo<'a>,
    address_space: &'a mut AddressSpace<'a>,
    total_cycles: usize
}

impl<'a> std::fmt::Debug for CPU<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("PC:{:04X?} A:{:02X?} X:{:02X?} Y:{:02X?} P:{:02X?} SP:{:02X?}",
                                 self.pc, self.acc, self.x, self.y, self.status.as_bytes(), self.sp))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AddressingMode {
    Immediate,
    Relative,
    Accumulator,
    Absolute, AbsoluteX, AbsoluteY,
    ZeroPage, ZeroPageX, ZeroPageY,
    Indirect, IndirectX, IndirectY,
    Implicit
}

use AddressingMode::*;
use std::fmt::Formatter;

const OPCODES : [(&str, Opcode, Cycles, AddressingMode); 240] = [
    ("BRK", 0x00, 7, Implicit), ("ORA", 0x01, 6, IndirectX), ("ORA", 0x05, 3, ZeroPage),
    ("ASL", 0x06, 5, ZeroPage), ("PHP", 0x08, 3, Implicit), ("ORA", 0x09, 2, Immediate),
    ("ASL", 0x0A, 2, Accumulator), ("ORA", 0x0D, 4, Absolute), ("ASL", 0x0E, 6, Absolute),
    ("BPL", 0x10, 2, Relative), ("ORA", 0x11, 5, IndirectY), ("ORA", 0x15, 4, ZeroPageX),
    ("ASL", 0x16, 6, ZeroPageX), ("CLC", 0x18, 2, Implicit), ("ORA", 0x19, 4, AbsoluteY),
    ("ORA", 0x1D, 4, AbsoluteX), ("ASL", 0x1E, 7, AbsoluteX), ("JSR", 0x20, 6, Absolute),
    ("AND", 0x21, 6, IndirectX), ("BIT", 0x24, 3, ZeroPage), ("AND", 0x25, 3, ZeroPage),
    ("ROL", 0x26, 5, ZeroPage), ("PLP", 0x28, 4, Implicit), ("AND", 0x29, 2, Immediate),
    ("ROL", 0x2A, 2, Accumulator), ("BIT", 0x2C, 4, Absolute), ("AND", 0x2D, 4, Absolute),
    ("ROL", 0x2E, 6, Absolute), ("BMI", 0x30, 2, Relative), ("AND", 0x31, 5, IndirectY),
    ("AND", 0x35, 4, ZeroPageX), ("ROL", 0x36, 6, ZeroPageX), ("SEC", 0x38, 2, Implicit),
    ("AND", 0x39, 4, AbsoluteY), ("AND", 0x3D, 4, AbsoluteX), ("ROL", 0x3E, 7, AbsoluteX),
    ("RTI", 0x40, 6, Implicit), ("EOR", 0x41, 6, IndirectX), ("EOR", 0x45, 3, ZeroPage),
    ("LSR", 0x46, 5, ZeroPage), ("PHA", 0x48, 3, Implicit), ("EOR", 0x49, 2, Immediate),
    ("LSR", 0x4A, 2, Accumulator), ("JMP", 0x4C, 3, Absolute), ("EOR", 0x4D, 4, Absolute),
    ("LSR", 0x4E, 6, Absolute), ("BVC", 0x50, 2, Relative), ("EOR", 0x51, 5, IndirectY),
    ("EOR", 0x55, 4, ZeroPageX), ("LSR", 0x56, 6, ZeroPageX), ("CLI", 0x58, 2, Implicit),
    ("EOR", 0x59, 4, AbsoluteY), ("EOR", 0x5D, 4, AbsoluteX), ("LSR", 0x5E, 7, AbsoluteX),
    ("RTS", 0x60, 6, Implicit), ("ADC", 0x61, 6, IndirectX), ("ADC", 0x65, 3, ZeroPage),
    ("ROR", 0x66, 5, ZeroPage), ("PLA", 0x68, 4, Implicit), ("ADC", 0x69, 2, Immediate),
    ("ROR", 0x6A, 2, Accumulator), ("JMP", 0x6C, 5, Indirect), ("ADC", 0x6D, 4, Absolute),
    ("ROR", 0x6E, 6, Absolute), ("BVS", 0x70, 2, Relative), ("ADC", 0x71, 5, IndirectY),
    ("ADC", 0x75, 4, ZeroPageX), ("ROR", 0x76, 6, ZeroPageX), ("SEI", 0x78, 2, Implicit),
    ("ADC", 0x79, 4, AbsoluteY), ("ADC", 0x7D, 4, AbsoluteX), ("ROR", 0x7E, 7, AbsoluteX),
    ("STA", 0x81, 6, IndirectX), ("STY", 0x84, 3, ZeroPage), ("STA", 0x85, 3, ZeroPage),
    ("STX", 0x86, 3, ZeroPage), ("DEY", 0x88, 2, Implicit), ("TXA", 0x8A, 2, Implicit),
    ("STY", 0x8C, 4, Absolute), ("STA", 0x8D, 4, Absolute), ("STX", 0x8E, 4, Absolute),
    ("BCC", 0x90, 2, Relative), ("STA", 0x91, 6, IndirectY), ("STY", 0x94, 4, ZeroPageX),
    ("STA", 0x95, 4, ZeroPageX), ("STX", 0x96, 4, ZeroPageY), ("TYA", 0x98, 2, Implicit),
    ("STA", 0x99, 5, AbsoluteY), ("TXS", 0x9A, 2, Implicit), ("STA", 0x9D, 5, AbsoluteX),
    ("LDY", 0xA0, 2, Immediate), ("LDA", 0xA1, 6, IndirectX), ("LDX", 0xA2, 2, Immediate),
    ("LDY", 0xA4, 3, ZeroPage), ("LDA", 0xA5, 3, ZeroPage), ("LDX", 0xA6, 3, ZeroPage),
    ("TAY", 0xA8, 2, Implicit), ("LDA", 0xA9, 2, Immediate), ("TAX", 0xAA, 2, Implicit),
    ("LDY", 0xAC, 4, Absolute), ("LDA", 0xAD, 4, Absolute), ("LDX", 0xAE, 4, Absolute),
    ("BCS", 0xB0, 2, Relative), ("LDA", 0xB1, 5, IndirectY), ("LDY", 0xB4, 4, ZeroPageX),
    ("LDA", 0xB5, 4, ZeroPageX), ("LDX", 0xB6, 4, ZeroPageY), ("CLV", 0xB8, 2, Implicit),
    ("LDA", 0xB9, 4, AbsoluteY), ("TSX", 0xBA, 2, Implicit), ("LDY", 0xBC, 4, AbsoluteX),
    ("LDA", 0xBD, 4, AbsoluteX), ("LDX", 0xBE, 4, AbsoluteY), ("CPY", 0xC0, 2, Immediate),
    ("CMP", 0xC1, 6, IndirectX), ("CPY", 0xC4, 3, ZeroPage), ("CMP", 0xC5, 3, ZeroPage),
    ("DEC", 0xC6, 5, ZeroPage), ("INY", 0xC8, 2, Implicit), ("CMP", 0xC9, 2, Immediate),
    ("DEX", 0xCA, 2, Implicit), ("CPY", 0xCC, 4, Absolute), ("CMP", 0xCD, 4, Absolute),
    ("DEC", 0xCE, 6, Absolute), ("BNE", 0xD0, 2, Relative), ("CMP", 0xD1, 5, IndirectY),
    ("CMP", 0xD5, 4, ZeroPageX), ("DEC", 0xD6, 6, ZeroPageX), ("CLD", 0xD8, 2, Implicit),
    ("CMP", 0xD9, 4, AbsoluteY), ("CMP", 0xDD, 4, AbsoluteX), ("DEC", 0xDE, 7, AbsoluteX),
    ("CPX", 0xE0, 2, Immediate), ("SBC", 0xE1, 6, IndirectX), ("CPX", 0xE4, 3, ZeroPage),
    ("SBC", 0xE5, 3, ZeroPage), ("INC", 0xE6, 5, ZeroPage), ("INX", 0xE8, 2, Implicit),
    ("SBC", 0xE9, 2, Immediate), ("NOP", 0xEA, 2, Implicit), ("CPX", 0xEC, 4, Absolute),
    ("SBC", 0xED, 4, Absolute), ("INC", 0xEE, 6, Absolute), ("BEQ", 0xF0, 2, Relative),
    ("SBC", 0xF1, 5, IndirectY), ("SBC", 0xF5, 4, ZeroPageX), ("INC", 0xF6, 6, ZeroPageX),
    ("SED", 0xF8, 2, Implicit), ("SBC", 0xF9, 4, AbsoluteY), ("SBC", 0xFD, 4, AbsoluteX),
    ("INC", 0xFE, 7, AbsoluteX),
    // Unofficial opcodes
    ("ASO", 0x0F, 6, Absolute), ("ASO", 0x1F, 7, AbsoluteX), ("ASO", 0x1B, 7, AbsoluteY),
    ("ASO", 0x07, 5, ZeroPage), ("ASO", 0x17, 6, ZeroPageX), ("ASO", 0x03, 8, IndirectX),
    ("ASO", 0x13, 8, IndirectY),
    ("NOP", 0x04, 3, Immediate), ("NOP", 0x44, 3, Immediate), ("NOP", 0x64, 3, Immediate),
    ("SKW", 0x0C, 4, Absolute), ("NOP", 0x14, 4, Immediate), ("NOP", 0x34, 4, Immediate),
    ("NOP", 0x54, 4, Immediate), ("NOP", 0x74, 4, Immediate), ("NOP", 0xD4, 4, Immediate),
    ("NOP", 0xF4, 4, Immediate), ("NOP", 0x1A, 2, Implicit), ("NOP", 0x3A, 2, Implicit),
    ("NOP", 0x5A, 2, Implicit), ("NOP", 0x7A, 2, Implicit), ("NOP", 0xDA, 2, Implicit),
    ("NOP", 0xFA, 2, Implicit), ("NOP", 0x80, 2, Immediate), ("SKW", 0x1C, 4, AbsoluteX),
    ("SKW", 0x3C, 4, AbsoluteX), ("SKW", 0x5C, 4, AbsoluteX), ("SKW", 0x7C, 4, AbsoluteX),
    ("SKW", 0xDC, 4, AbsoluteX), ("SKW", 0xFC, 4, AbsoluteX),
    ("RLA", 0x2F, 6, Absolute), ("RLA", 0x3F, 7, AbsoluteX), ("RLA", 0x3B, 7, AbsoluteY),
    ("RLA", 0x27, 5, ZeroPage), ("RLA", 0x37, 6, ZeroPageX), ("RLA", 0x23, 8, IndirectX),
    ("RLA", 0x33, 8, IndirectY),
    ("LSE", 0x4F, 6, Absolute), ("LSE", 0x5F, 7, AbsoluteX), ("LSE", 0x5B, 7, AbsoluteY),
    ("LSE", 0x47, 5, ZeroPage), ("LSE", 0x57, 6, ZeroPageX), ("LSE", 0x43, 8, IndirectX),
    ("LSE", 0x53, 8, IndirectY),
    ("RRA", 0x6F, 6, Absolute), ("RRA", 0x7F, 7, AbsoluteX), ("RRA", 0x7B, 7, AbsoluteY),
    ("RRA", 0x67, 5, ZeroPage), ("RRA", 0x77, 6, ZeroPageX), ("RRA", 0x63, 8, IndirectX),
    ("RRA", 0x73, 8, IndirectY),
    ("AXS", 0x8F, 4, Absolute), ("AXS", 0x87, 3, ZeroPage), ("AXS", 0x97, 4, ZeroPageY),
    ("AXS", 0x83, 6, IndirectX),
    ("LAX", 0xAF, 4, Absolute), ("LAX", 0xBF, 4, AbsoluteY), ("LAX", 0xA7, 3, ZeroPage),
    ("LAX", 0xB7, 4, ZeroPageY), ("LAX", 0xA3, 6, IndirectX), ("LAX", 0xB3, 5, IndirectY),
    ("DCM", 0xCF, 6, Absolute), ("DCM", 0xDF, 7, AbsoluteX), ("DCM", 0xDB, 7, AbsoluteY),
    ("DCM", 0xC7, 5, ZeroPage), ("DCM", 0xD7, 6, ZeroPageX), ("DCM", 0xC3, 8, IndirectX),
    ("DCM", 0xD3, 8, IndirectY),
    ("INS", 0xEF, 6, Absolute), ("INS", 0xFF, 7, AbsoluteX), ("INS", 0xFB, 7, AbsoluteY),
    ("INS", 0xE7, 5, ZeroPage), ("INS", 0xF7, 6, ZeroPageX), ("INS", 0xE3, 8, IndirectX),
    ("INS", 0xF3, 8, IndirectY),
    ("ALR", 0x4B, 2, Immediate), ("ARR", 0x6B, 2, Immediate), ("XAA", 0x8B, 2, Immediate),
    ("OAL", 0xAB, 2, Immediate), ("SAX", 0xCB, 2, Immediate), ("TAS", 0x9B, 5, AbsoluteY),
    ("SAY", 0x9C, 5, AbsoluteX), ("XAS", 0x9E, 5, AbsoluteY), ("AXA", 0x9F, 5, AbsoluteY),
    ("AXA", 0x93, 6, IndirectY), ("ANC", 0x2B, 2, Immediate), ("ANC", 0x0B, 2, Immediate),
    ("LAS", 0xBB, 4, AbsoluteY), ("SBC", 0xEB, 2, Immediate)
];

const OPCODES_EXTRA_CYCLES_ON_PAGE: [u8; 32] = [
    0x3D, 0x39, 0x31, 0x7D, 0x79, 0x71, 0xDD, 0xD9, 0xD1, 0x5D, 0x59, 0x51, 0xBD, 0xB9, 0xB1, 0xBE,
    0xBC, 0x1D, 0x19, 0x11, 0xFD, 0xF9, 0xF1, 0xBF, 0xB3, 0xBB, 0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC
];

const BRANCH_EXTRA_CYCLES_ON_PAGE: [u8; 8] = [
    0x90, 0xB0, 0xF0, 0x30, 0xD0, 0x10, 0x50, 0x70
];

impl<'a> CPU<'a> {

    pub fn new(address_space: &'a mut AddressSpace<'a>) -> CPU<'a> {
        // TODO: Support other memory mappers...
        // Abstract away the following into another component...

        // The reset vector is at 0xFFFC and 0xFFFD in the CPU memory.
        let pc = address_space.read_data_u16(0xFFFC);
        let opcode_map: HashMap<Opcode, (&str, Opcode, Cycles, AddressingMode)> =
            OPCODES.iter().map(|t| (t.1, *t)).collect();

        CPU { pc, acc: 0, x: 0, y: 0, status: StatusRegister::new(), sp: 0xFF,
            opcode_map, address_space, total_cycles: 7 }
    }

    fn address_mode_to_register(&self, address_mode: AddressingMode) -> u8 {
        match address_mode {
            AbsoluteX | ZeroPageX => self.x,
            AbsoluteY | ZeroPageY => self.y,
            ZeroPage => 0,
            _ => panic!("No other address modes match to a register")
        }
    }

    fn check_page_boundary_crossed(&self, address_mode: AddressingMode) -> usize {
        match address_mode {
            AbsoluteX | AbsoluteY => {
                let reg = self.address_mode_to_register(address_mode);
                let addr = self.address_space.read_data_u16(self.pc.wrapping_add(1));
                let addr_low = addr as u8;
                let addr_added = ((addr_low as u16) + (reg as u16)) & 0xFF;

                if addr_added < reg as u16 {
                    // Wrapped around (have to carry to the high byte). This will result in an
                    // extra cycle.
                    1
                } else {
                    0
                }
            }
            IndirectY => {
                let zero_page_addr =
                    self.address_space.read_data_u8(self.pc.wrapping_add(1));
                let addr = self.address_space.read_data_u16(zero_page_addr as u16);
                let addr_low = addr as u8;
                let addr_added = ((addr_low as u16) + (self.y as u16)) & 0xFF;

                if addr_added < self.y as u16 {
                    // Wrapped around (have to carry to the high byte). This will result in an
                    // extra cycle.
                    1
                } else {
                    0
                }
            }
            Relative => {
                let src = self.address_space.read_data_u8(self.pc.wrapping_add(1));
                let inc_pc = self.pc.wrapping_add(self.num_bytes_from_mode(address_mode));
                if (inc_pc & 0xFF00) != (self.rel_address(src)
                    .wrapping_add(self.num_bytes_from_mode(address_mode)) & 0xFF00) {
                    1
                } else {
                    0
                }
            }
            _ => 0
        }
    }

    fn get_addr_by_address_mode(&self, address_mode: AddressingMode) -> u16 {
        match address_mode {
            Absolute => {
                self.address_space.read_data_u16(self.pc.wrapping_add(1))
            }
            AbsoluteX | AbsoluteY => {
                let reg = self.address_mode_to_register(address_mode);
                let addr = self.address_space.read_data_u16(self.pc.wrapping_add(1));
                let val = addr.wrapping_add(reg as u16);

                val
            }
            ZeroPage | ZeroPageX | ZeroPageY => {
                let addr =
                    self.address_space.read_data_u8(self.pc.wrapping_add(1)) as u16;
                // self.address_mode_to_register returns 0 if ZeroPage
                let added = addr.wrapping_add(self.address_mode_to_register(address_mode) as u16);

                added & 0x00FF
            }
            IndirectX => {
                let addr = self.address_space.read_data_u8(self.pc.wrapping_add(1));
                let with_x = addr.wrapping_add(self.x);
                let target = self.address_space.read_u16_zero_page(with_x);

                target
            }
            IndirectY => {
                let zero_page_addr =
                    self.address_space.read_data_u8(self.pc.wrapping_add(1));
                let addr = self.address_space.read_u16_zero_page(zero_page_addr);
                let with_y = addr.wrapping_add(self.y as u16);

                with_y
            }
            _ => {
                panic!("No other address modes support returning address to write data to");
            }
        }
    }

    fn get_src_by_address_mode(&self, address_mode: AddressingMode) -> u8 {
        match address_mode {
            Immediate | Relative => {
                self.address_space.read_data_u8(self.pc.wrapping_add(1))
            }
            Absolute | AbsoluteX | AbsoluteY | ZeroPage | ZeroPageX | ZeroPageY | IndirectX
                | IndirectY => {
                let addr = self.get_addr_by_address_mode(address_mode);
                self.address_space.read_data_u8(addr)
            }
            Accumulator => {
                self.acc
            }
            Indirect => {
                panic!("Indirect address mode does not return a u8 value");

            }
            Implicit => {
                panic!("Implicit mode requires no operands");
            }
        }
    }

    fn num_bytes_from_mode(&self, address_mode: AddressingMode) -> u16 {
        match address_mode {
            Accumulator | Implicit => 1,
            Absolute | AbsoluteX | AbsoluteY | Indirect => 3,
            _ => 2
        }
    }

    pub fn execute_next_instruction(&mut self) -> Cycles {
        let opcode = self.address_space.read_data_u8(self.pc);

        let instruction_info = self.opcode_map.get(&opcode).unwrap();

        let operand_start = self.pc.wrapping_add(1);
        let next_byte = self.address_space.read_data_u8(operand_start);
        let next_two_bytes = self.address_space.read_data_u16(operand_start);

        println!("{} {:<30} {:?}    CYC:{}    Stack (top 6): {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}",
            self.print_machine_code(instruction_info, next_byte, next_two_bytes),
            self.print_assembly_command(instruction_info, next_byte, next_two_bytes),
            self, self.total_cycles,
            self.peek(1), self.peek(2), self.peek(3), self.peek(4), self.peek(5), self.peek(6));

        let addr_mode = instruction_info.3;
        let page_cycles = if OPCODES_EXTRA_CYCLES_ON_PAGE.contains(&instruction_info.1) {
            self.check_page_boundary_crossed(addr_mode)
        } else {
            0
        };

        let mut cycles = instruction_info.2 + page_cycles;

        let next_pc: u16 = match instruction_info.0 {
            "ADC" => {
                self.adc(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ALR" => {
                self.and(addr_mode);
                self.lsr(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ANC" => {
                let src = self.and(addr_mode);
                self.status.carry_flag = ((src >> 7) & 1) == 1;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "AND" => {
                self.and(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ARR" => {
                self.and(addr_mode);
                self.ror(Accumulator);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ASL" => {
                self.asl(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ASO" => {
                self.asl(addr_mode);
                self.ora(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "AXA" => {
                let target_addr = self.get_addr_by_address_mode(addr_mode);
                let target_hi = (target_addr >> 8) as u8;
                let final_result = (target_hi.wrapping_add(1)) & self.x & self.acc;

                self.address_space.write_data(target_addr, final_result);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "AXS" => {
                let anded_val = self.acc & self.x;
                self.address_space.write_data(self.get_addr_by_address_mode(
                    addr_mode), anded_val);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "BCC" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| !self.status.carry_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BCS" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| self.status.carry_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BEQ" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| self.status.zero_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BIT" => {
                let val = self.get_src_by_address_mode(addr_mode);
                self.set_sign(val);
                self.status.overflow_flag = (0x40 & val) > 0;
                self.set_zero(val & self.acc);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "BMI" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| self.status.negative_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BNE" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| !self.status.zero_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BPL" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| !self.status.negative_flag, addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BRK" => {
                self.pc = self.pc.wrapping_add(1);
                let pc_hi = (self.pc >> 8) & 0xFF;
                let pc_lo = self.pc & 0xFF;
                self.push(pc_hi as u8);
                self.push(pc_lo as u8);
                self.status.break_flag_1 = true;
                self.push(self.status.as_bytes());
                // Break flag 1 is only true on the stack.. Set it back to false
                self.status.break_flag_1 = false;
                self.status.interrupt_disable_flag = true;

                self.address_space.read_data_u16(0xFFFE)
            }
            "BVC" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| !self.status.overflow_flag,
                                         addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "BVS" => {
                let (add_cycles, new_pc) =
                    self.branch_on_condition(&|| self.status.overflow_flag,
                                         addr_mode, &instruction_info.1);
                cycles += add_cycles;
                new_pc
            }
            "CLC" => {
                self.status.carry_flag = false;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CLD" => {
                self.status.decimal_flag = false;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CLI" => {
                self.status.interrupt_disable_flag = false;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CLV" => {
                self.status.overflow_flag = false;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CMP" => {
                self.cmp(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CPX" => {
                let val = self.get_src_by_address_mode(addr_mode);
                let cmp_x = (self.x as u16).wrapping_sub(val as u16);
                self.set_carry((cmp_x < 0x100) as u8);
                self.set_sign(cmp_x as u8);
                self.set_zero((cmp_x as u8) & 0xFF);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "CPY" => {
                let val = self.get_src_by_address_mode(addr_mode);
                let cmp_y = (self.y as u16).wrapping_sub(val as u16);
                self.set_carry((cmp_y < 0x100) as u8);
                self.set_sign(cmp_y as u8);
                self.set_zero((cmp_y as u8) & 0xFF);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "DCM" => {
                self.dec(addr_mode);
                self.cmp(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "DEC" => {
                self.dec(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "DEX" => {
                let x_val = self.x;
                let dec_val = x_val.wrapping_sub(1);
                self.set_sign(dec_val);
                self.set_zero(dec_val);

                self.x = dec_val;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "DEY" => {
                let y_val = self.y;
                let dec_val = y_val.wrapping_sub(1);
                self.set_sign(dec_val);
                self.set_zero(dec_val);

                self.y = dec_val;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "EOR" => {
                self.eor(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "INC" => {
                self.inc(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "INS" => {
                self.inc(addr_mode);
                self.sbc(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "INX" => {
                let x_val = self.x;
                let dec_val = x_val.wrapping_add(1);
                self.set_sign(dec_val);
                self.set_zero(dec_val);

                self.x = dec_val;
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "INY" => {
                let y_val = self.y;
                let dec_val = y_val.wrapping_add(1);
                self.set_sign(dec_val);
                self.set_zero(dec_val);

                self.y = dec_val;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "JMP" => {
                match addr_mode {
                    Absolute => {
                        let target =
                            self.address_space.read_data_u16(self.pc.wrapping_add(1));

                        target
                    }
                    Indirect => {
                        let addr = self.address_space.read_data_u16(self.pc.wrapping_add(1));

                        let target;
                        if (addr as u8) == 0xFF {
                            // This is a 'booby-trap' in the 6502. For indirect JMPs to addresses
                            // of (xxFF), the most significant byte will be fetched from xx00 instead
                            // of the next page
                            let low_byte = self.address_space.read_data_u8(addr);
                            let high_byte = self.address_space.read_data_u8(addr & 0xFF00);

                            target = (low_byte as u16) | ((high_byte as u16) << 8)
                        } else {
                            target = self.address_space.read_data_u16(addr);
                        }


                        target
                    }
                    _ => { unimplemented!("Unsupported addressing mode for JMP") }
                }
            }
            "JSR" => {
                let addr = self.get_addr_by_address_mode(addr_mode);

                let to_push_pc =
                    self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
                        .wrapping_sub(1);

                let pc_hi = (to_push_pc >> 8) & 0xFF;
                let pc_lo = to_push_pc & 0xFF;
                self.push(pc_hi as u8);
                self.push(pc_lo as u8);

                addr
            }
            "LAS" => {
                let val = self.get_src_by_address_mode(addr_mode);
                let anded = val & self.sp;
                self.acc = anded;
                self.x = anded;
                self.sp = anded;

                self.set_sign(anded);
                self.set_zero(anded);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LAX" => {
                self.lda(addr_mode);
                self.ldx(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LDA" => {
                self.lda(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LDX" => {
                self.ldx(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LDY" => {
                let val = self.get_src_by_address_mode(addr_mode);
                self.y = val;
                self.set_sign(val);
                self.set_zero(val);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LSE" => {
                self.lsr(addr_mode);
                self.eor(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "LSR" => {
                self.lsr(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "NOP" => {
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "OAL" => {
                let ored = 0xEE | self.acc;
                self.set_sign(ored);
                self.set_zero(ored);
                self.acc = ored;

                self.and(addr_mode);
                self.tax();

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ORA" => {
                self.ora(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "PHA" => {
                self.push(self.acc);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "PHP" => {
                self.push(self.status.as_bytes());

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "PLA" => {
                let val = self.pull();
                self.acc = val;
                self.set_sign(val);
                self.set_zero(val);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "PLP" => {
                let val = self.pull();
                self.status = self.status.from_bytes(val);
                self.status.break_flag_1 = false;
                self.status.break_flag_2 = true;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "RLA" => {
                self.rol(addr_mode);
                self.and(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ROL" => {
                self.rol(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "ROR" => {
                self.ror(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "RRA" => {
                self.ror(addr_mode);
                self.adc(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "RTI" => {
                let sr = self.pull();
                self.status = self.status.from_bytes(sr);
                self.status.break_flag_1 = false;
                self.status.break_flag_2 = true;

                let addr1 = self.pull() as u16;
                let addr2 = self.pull() as u16;

                addr1 | (addr2 << 8)
            }
            "RTS" => {
                let src = (self.pull() as u16).wrapping_add(((self.pull() as u16) << 8)
                    .wrapping_add(1));

                src
            }
            "SAX" => {
                let anded = self.acc & self.x;
                let tmp_acc = self.acc;
                self.acc = anded;
                self.set_carry(1);

                self.sbc(addr_mode);
                self.x = self.acc;

                self.acc = tmp_acc;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SAY" => {
                let target_addr = self.get_addr_by_address_mode(addr_mode);
                let target_hi = (target_addr >> 8) as u8;
                let final_result = (target_hi.wrapping_add(1)) & self.y;

                self.address_space.write_data(target_addr, final_result);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SBC" => {
                self.sbc(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SEC" => {
                self.set_carry(1);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SED" => {
                self.status.decimal_flag = true;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SEI" => {
                self.status.interrupt_disable_flag = true;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "SKW" => {
                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "STA" => {
                self.address_space.write_data(self.get_addr_by_address_mode(
                    addr_mode), self.acc);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "STX" => {
                self.address_space.write_data(self.get_addr_by_address_mode(
                    addr_mode), self.x);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "STY" => {
                self.address_space.write_data(self.get_addr_by_address_mode(
                    addr_mode), self.y);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TAS" => {
                let anded = self.x & self.acc;
                self.sp = anded;

                let target_addr = self.get_addr_by_address_mode(addr_mode);
                let target_hi = (target_addr >> 8) as u8;
                let final_result = (target_hi.wrapping_add(1)) & anded;

                self.address_space.write_data(target_addr, final_result);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TAX" => {
                self.tax();

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TAY" => {
                self.y = self.acc;
                self.set_sign(self.y);
                self.set_zero(self.y);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TSX" => {
                self.x = self.sp;
                self.set_sign(self.x);
                self.set_zero(self.x);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TXA" => {
                self.txa();

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TXS" => {
                self.sp = self.x;

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "TYA" => {
                self.acc = self.y;
                self.set_sign(self.y);
                self.set_zero(self.y);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "XAA" => {
                self.txa();
                self.and(addr_mode);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            "XAS" => {
                let target_addr = self.get_addr_by_address_mode(addr_mode);
                let target_hi = (target_addr >> 8) as u8;
                let final_result = (target_hi.wrapping_add(1)) & self.x;

                self.address_space.write_data(target_addr, final_result);

                self.pc.wrapping_add(self.num_bytes_from_mode(addr_mode))
            }
            _ => {
                unimplemented!("Unknown instruction");
            }
        };

        self.pc = next_pc;

        self.total_cycles = self.total_cycles + cycles;

        cycles
    }

    fn push(&mut self, byte: u8) {
        self.address_space.write_data((0x0100 as u16).wrapping_add(self.sp as u16), byte);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pull(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.address_space.read_data_u8((0x0100 as u16).wrapping_add(self.sp as u16))
    }

    fn peek(&self, n: u8) -> u8 {
        let inc_sp = self.sp.wrapping_add(n) as u16;
        self.address_space.read_data_u8((0x0100 as u16).wrapping_add(inc_sp))
    }

    fn branch_on_condition(&self, condition: &dyn Fn() -> bool,
                           address_mode: AddressingMode,
                           opcode: &u8) -> (usize, u16) {
        let val = self.get_src_by_address_mode(address_mode);
        if condition() {
            // One extra cycle for taking the branch, possibly another for crossing the page
            let page_cycles = if BRANCH_EXTRA_CYCLES_ON_PAGE.contains(opcode) {
                self.check_page_boundary_crossed(address_mode)
            } else {
                0
            };

            (1 + page_cycles,
                self.rel_address(val).wrapping_add(self.num_bytes_from_mode(address_mode)))
        } else {
            (0, self.pc.wrapping_add(self.num_bytes_from_mode(address_mode)))
        }
    }

    fn rel_address(&self, offset: u8) -> u16 {
        if offset >> 7 == 1 {
            // Negative - use two's complement to get the offset
            let adjusted_offset = (!offset).wrapping_add(1);
            self.pc.wrapping_sub(adjusted_offset as u16)
        } else {
            self.pc.wrapping_add(offset as u16)
        }
    }

    fn set_sign(&mut self, src: u8) {
        self.status.negative_flag = ((src >> 7) & 1) == 1;
    }

    fn set_zero(&mut self, src: u8) {
        self.status.zero_flag = src == 0;
    }

    fn set_carry(&mut self, condition: u8) {
        if condition != 0 {
            self.status.carry_flag = true;
        } else {
            self.status.carry_flag = false;
        }
    }

    fn print_machine_code(&self, instruction_info: &(&str, Opcode, Cycles, AddressingMode),
                              operand_u8: u8, operand_u16: u16) -> String {
        let (_, opcode, _, address_mode) = instruction_info;

        match address_mode {
            Implicit | Accumulator => {
                format!("{:02X}      ", opcode)
            }
            Absolute | AbsoluteX | AbsoluteY | Indirect => {
                format!("{:02X} {:02X} {:02X}", opcode, operand_u16 & 0x00FF, operand_u16 >> 8)
            }
            _ => {
                format!("{:02X} {:02X}   ", opcode, operand_u8)
            }
        }
    }

    fn print_assembly_command(&self, instruction_info: &(&str, Opcode, Cycles, AddressingMode),
                              operand_u8: u8, operand_u16: u16) -> String {

        let (instr, _, _, address_mode) = instruction_info;

        match address_mode {
            Immediate => {
                format!("{} #${:02X}", instr, operand_u8)
            }
            Relative => {
                format!("{} ${:02X}", instr, operand_u8)
            }
            Accumulator => {
                format!("{} A", instr)
            }
            Absolute => {
                format!("{} ${:04X}", instr, operand_u16)
            }
            AbsoluteX => {
                format!("{} ${:04X},X", instr, operand_u16)
            }
            AbsoluteY => {
                format!("{} ${:04X},Y", instr, operand_u16)
            }
            ZeroPage => {
                format!("{} ${:02X}", instr, operand_u8)
            }
            ZeroPageX => {
                format!("{} ${:02X},X", instr, operand_u8)
            }
            ZeroPageY => {
                format!("{} ${:02X},Y", instr, operand_u8)
            }
            Indirect => {
                format!("{} (${:04X})", instr, operand_u16)
            }
            IndirectX => {
                format!("{} (${:02X}, X)", instr, operand_u8)
            }
            IndirectY => {
                format!("{} (${:02X}, Y)", instr, operand_u8)
            }
            Implicit => {
                format!("{}", instr)
            }
        }
    }

    fn asl(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        self.set_carry(val & 0x80);
        let new_val = (val << 1) & 0xff;
        self.set_sign(new_val);
        self.set_zero(new_val);

        if addr_mode == AddressingMode::Accumulator {
            self.acc = new_val;
        } else {
            self.address_space.write_data(self.get_addr_by_address_mode(
                addr_mode), new_val);
        }

        new_val
    }

    fn rol(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode) as u16;
        let shifted = (val << 1) | (self.status.carry_flag as u16);
        self.set_carry((shifted > 0xff) as u8);
        self.set_sign(shifted as u8);
        self.set_zero(shifted as u8);

        if addr_mode == AddressingMode::Accumulator {
            self.acc = shifted as u8;
        } else {
            self.address_space.write_data(self.get_addr_by_address_mode(
                addr_mode), shifted as u8);
        }

        shifted as u8
    }

    fn and(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let new_acc = val & self.acc;

        self.set_sign(new_acc);
        self.set_zero(new_acc);

        self.acc = new_acc;

        new_acc
    }

    fn ora(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let ored = val | self.acc;
        self.set_sign(ored);
        self.set_zero(ored);
        self.acc = ored;

        ored
    }

    fn lsr(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        self.set_carry(val & 0x01);
        let shifted = val >> 1;
        self.set_sign(shifted);
        self.set_zero(shifted);

        if addr_mode == AddressingMode::Accumulator {
            self.acc = shifted;
        } else {
            self.address_space.write_data(self.get_addr_by_address_mode(
                addr_mode), shifted);
        }

        shifted
    }

    fn eor(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let xored = val ^ self.acc;
        self.set_sign(xored);
        self.set_zero(xored);

        self.acc = xored;

        xored
    }

    fn ror(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = (self.get_src_by_address_mode(addr_mode) as u16)
            | ((self.status.carry_flag as u16) << 8);
        self.set_carry((val as u8) & 0x01);
        let shifted = (val >> 1) as u8;
        self.set_sign(shifted);
        self.set_zero(shifted);

        if addr_mode == AddressingMode::Accumulator {
            self.acc = shifted;
        } else {
            self.address_space.write_data(self.get_addr_by_address_mode(
                addr_mode), shifted);
        }

        shifted
    }

    fn adc(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let tmp: u16 = (self.acc as u16).wrapping_add(val as u16)
            .wrapping_add(self.status.carry_flag as u16);

        self.set_sign(tmp as u8);
        self.set_zero(tmp as u8);
        self.status.overflow_flag = !(((self.acc ^ val) & 0x80) > 0) &&
            (((self.acc as u16 ^ tmp) & 0x80) > 0);
        self.status.carry_flag = tmp > 0xff;

        self.acc = tmp as u8;

        tmp as u8
    }

    fn lda(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        self.acc = val;
        self.set_sign(val);
        self.set_zero(val);

        val
    }

    fn ldx(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        self.x = val;
        self.set_sign(val);
        self.set_zero(val);

        val
    }

    fn dec(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let dec_val = val.wrapping_sub(1);
        self.set_sign(dec_val);
        self.set_zero(dec_val);

        self.address_space.write_data(self.get_addr_by_address_mode(
            addr_mode), dec_val);

        dec_val
    }

    fn cmp(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode) as u16;
        let src = (self.acc as u16).wrapping_sub(val);

        self.set_carry((src < 0x100) as u8);
        self.set_sign(src as u8);
        self.set_zero((src as u8) & 0xFF);

        src as u8
    }

    fn inc(&mut self, addr_mode: AddressingMode) -> u8 {
        let val = self.get_src_by_address_mode(addr_mode);
        let dec_val = val.wrapping_add(1);
        self.set_sign(dec_val);
        self.set_zero(dec_val);

        self.address_space.write_data(self.get_addr_by_address_mode(
            addr_mode), dec_val);

        dec_val
    }

    fn sbc(&mut self, addr_mode: AddressingMode) -> u8 {
        let src = self.get_src_by_address_mode(addr_mode);
        let tmp = (self.acc as u16).wrapping_sub(src as u16)
            .wrapping_sub(!self.status.carry_flag as u16);
        let u8_tmp = tmp as u8;
        self.set_sign(u8_tmp);
        self.set_zero(u8_tmp);
        self.status.overflow_flag = (((self.acc ^ u8_tmp) & 0x80) > 0) &&
            (((self.acc ^ src) & 0x80) > 0);
        self.set_carry((tmp < 0x100) as u8);
        self.acc = u8_tmp;

        u8_tmp
    }

    fn txa(&mut self) -> u8 {
        self.acc = self.x;
        self.set_sign(self.acc);
        self.set_zero(self.acc);

        self.acc
    }

    fn tax(&mut self) -> u8 {
        self.x = self.acc;
        self.set_sign(self.x);
        self.set_zero(self.x);

        self.x
    }
}