use std::{
    fmt::Debug,
    fs::File,
    io::{BufReader, Read},
};
macro_rules! set_zsp {
    ($c:expr, $val:expr) => {
        $c.zero_flag = ($val) == 0;
        $c.sign = ($val) >> 7 != 0;
        $c.parity = $c.parity($val);
    };
}

pub struct CPU {
    pub registers: [u8; 7],
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u16,
    pub memory: [u8; 0xFFFF + 1],
    interrupt_pending: bool,
    interrupt_delay: u8,
    interrupt_vector: u8,
    pub finished: bool,

    // Flags
    interupt_flip_flop: bool,
    interupt_delay: bool,
    halted: bool,
    carry_flag: bool,
    half_carry_flag: bool,
    zero_flag: bool,
    parity: bool,
    sign: bool,
}

impl CPU {
    pub fn new() -> Self {
        Self {
            status: 0,
            program_counter: 0x100,
            stack_pointer: 0,
            registers: [0; 7],
            memory: [0; 0xFFFF + 1],
            interrupt_delay: 0,
            interrupt_pending: false,
            interupt_flip_flop: false,
            interupt_delay: false,
            halted: false,
            carry_flag: false,
            half_carry_flag: false,
            zero_flag: false,
            parity: false,
            sign: false,
            finished: false,
            interrupt_vector: 0,
        }
    }

    pub fn load_file(&mut self, filename: &str) {
        let file = File::open(filename).unwrap();
        let mut reader = BufReader::new(file);
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).unwrap();
        self.memory[0x100..0x100 + buffer.len()].copy_from_slice(&buffer);
    }

    pub fn step(&mut self, counter: i32) {
        if self.interrupt_pending && self.interupt_flip_flop && self.interrupt_delay == 0 {
            self.interrupt_pending = false;
            self.interupt_delay = false;
            self.halted = false;
        } else if !self.halted {
            let byte = self.next_byte();
            self.execute(byte, counter);
        }
    }

    pub fn execute(&mut self, opcode: u8, counter: i32) {
        println!("Opcode {}: {:X}", counter, opcode);
        println!(
            "CPU State: A={} B={} C={} D={} E={} H={} L={}",
            self.registers[0],
            self.registers[1],
            self.registers[2],
            self.registers[3],
            self.registers[4],
            self.registers[5],
            self.registers[6]
        );
        println!("PC={} SP={}", self.program_counter, self.stack_pointer);
        println!(
            "Flags: SF={} ZF={} HF={} PF={} CF={}",
            self.sign as u8,
            self.zero_flag as u8,
            self.half_carry_flag as u8,
            self.parity as u8,
            self.carry_flag as u8
        );
        match opcode {
            0x7F => self.registers[0] = self.registers[0], // MOV A, A
            0x78 => self.registers[0] = self.registers[1], // MOV A, B
            0x79 => self.registers[0] = self.registers[2], // MOV A, C
            0x7A => self.registers[0] = self.registers[3], // MOV A, D
            0x7B => self.registers[0] = self.registers[4], // MOV A, E
            0x7C => self.registers[0] = self.registers[5], // MOV A, H
            0x7D => self.registers[0] = self.registers[6], // MOV A, L
            0x7E => self.registers[0] = self.read_byte(self.get_hl()), // MOV A, M

            0x0A => self.registers[0] = self.read_byte(self.get_bc()), // LDAX B
            0x1A => self.registers[0] = self.read_byte(self.get_de()), // LDAX D
            0x3A => {
                self.registers[0] = {
                    let address = self.next_word();
                    self.read_byte(address)
                }
            } // LDA word

            0x47 => self.registers[1] = self.registers[0], // MOV B, A
            0x40 => self.registers[1] = self.registers[1], // MOV B, B
            0x41 => self.registers[1] = self.registers[2], // MOV B, C
            0x42 => self.registers[1] = self.registers[3], // MOV B, D
            0x43 => self.registers[1] = self.registers[4], // MOV B, E
            0x44 => self.registers[1] = self.registers[5], // MOV B, H
            0x45 => self.registers[1] = self.registers[6], // MOV B, L
            0x46 => self.registers[1] = self.read_byte(self.get_hl()), // MOV B, M

            0x4F => self.registers[2] = self.registers[0], // MOV C, A
            0x48 => self.registers[2] = self.registers[1], // MOV C, B
            0x49 => self.registers[2] = self.registers[2], // MOV C, C
            0x4A => self.registers[2] = self.registers[3], // MOV C, D
            0x4B => self.registers[2] = self.registers[4], // MOV C, E
            0x4C => self.registers[2] = self.registers[5], // MOV C, H
            0x4D => self.registers[2] = self.registers[6], // MOV C, L
            0x4E => self.registers[2] = self.read_byte(self.get_hl()), // MOV C, M

            0x57 => self.registers[3] = self.registers[0], // MOV D, A
            0x50 => self.registers[3] = self.registers[1], // MOV D, B
            0x51 => self.registers[3] = self.registers[2], // MOV D, C
            0x52 => self.registers[3] = self.registers[3], // MOV D, D
            0x53 => self.registers[3] = self.registers[4], // MOV D, E
            0x54 => self.registers[3] = self.registers[5], // MOV D, H
            0x55 => self.registers[3] = self.registers[6], // MOV D, L
            0x56 => self.registers[3] = self.read_byte(self.get_hl()), // MOV D, M

            0x5F => self.registers[4] = self.registers[0], // MOV E, A
            0x58 => self.registers[4] = self.registers[1], // MOV E, B
            0x59 => self.registers[4] = self.registers[2], // MOV E, C
            0x5A => self.registers[4] = self.registers[3], // MOV E, D
            0x5B => self.registers[4] = self.registers[4], // MOV E, E
            0x5C => self.registers[4] = self.registers[5], // MOV E, H
            0x5D => self.registers[4] = self.registers[6], // MOV E, L
            0x5E => self.registers[4] = self.read_byte(self.get_hl()), // MOV E, M
            0x00 => {}

            0x67 => self.registers[5] = self.registers[0], // MOV H, A
            0x60 => self.registers[5] = self.registers[1], // MOV H, B
            0x61 => self.registers[5] = self.registers[2], // MOV H, C
            0x62 => self.registers[5] = self.registers[3], // MOV H, D
            0x63 => self.registers[5] = self.registers[4], // MOV H, E
            0x64 => self.registers[5] = self.registers[5], // MOV H, H
            0x65 => self.registers[5] = self.registers[6], // MOV H, L
            0x66 => self.registers[5] = self.read_byte(self.get_hl()), // MOV H, M                0x5F => self.registers[4] = self.registers[0], // MOV E, A

            0x6F => self.registers[6] = self.registers[0], // MOV L, A
            0x68 => self.registers[6] = self.registers[1], // MOV L, B
            0x69 => self.registers[6] = self.registers[2], // MOV L, C
            0x6A => self.registers[6] = self.registers[3], // MOV L, D
            0x6B => self.registers[6] = self.registers[4], // MOV L, E
            0x6C => self.registers[6] = self.registers[5], // MOV L, H
            0x6D => self.registers[6] = self.registers[6], // MOV L, L
            0x6E => self.registers[6] = self.read_byte(self.get_hl()), // MOV L, M

            0x77 => self.write_byte(self.get_hl(), self.registers[0]), // MOV M, A
            0x70 => self.write_byte(self.get_hl(), self.registers[1]), // MOV M, B
            0x71 => self.write_byte(self.get_hl(), self.registers[2]), // MOV M, C
            0x72 => self.write_byte(self.get_hl(), self.registers[3]), // MOV M, D
            0x73 => self.write_byte(self.get_hl(), self.registers[4]), // MOV M, E
            0x74 => self.write_byte(self.get_hl(), self.registers[5]), // MOV M, H
            0x75 => self.write_byte(self.get_hl(), self.registers[6]), // MOV M, L

            0x3E => self.registers[0] = self.next_byte(), // MVI A, byte
            0x06 => self.registers[1] = self.next_byte(), // MVI B, byte
            0x0E => self.registers[2] = self.next_byte(), // MVI C, byte
            0x16 => self.registers[3] = self.next_byte(), // MVI D, byte
            0x1E => self.registers[4] = self.next_byte(), // MVI E, byte
            0x26 => self.registers[5] = self.next_byte(), // MVI H, byte
            0x2E => self.registers[6] = self.next_byte(), // MVI L, byte
            0x36 => {
                let byte = self.next_byte();
                self.write_byte(self.get_hl(), byte)
            } // MVI M, byte

            0x02 => self.write_byte(self.get_bc(), self.registers[0]), // STAX B
            0x12 => self.write_byte(self.get_de(), self.registers[0]), // STAX D
            0x32 => {
                let address = self.next_word();
                self.write_byte(address, self.registers[0])
            } // STA word

            0x01 => {
                let word = self.next_word();
                self.set_bc(word);
            } // LXI B, word
            0x11 => {
                let word = self.next_word();
                self.set_de(word);
            } // LXI D, word
            0x21 => {
                let word = self.next_word();
                self.set_hl(word);
            } // LXI H, word
            0x31 => self.stack_pointer = self.next_word(), // LXI SP, word
            0x2A => {
                let word = self.next_word();
                let value = self.read_word(word);
                self.set_hl(value);
            } // LHLD
            0x22 => {
                let value = self.get_hl();
                let word = self.next_word();
                self.write_word(word, value);
            } // SHLD
            0xF9 => self.stack_pointer = self.get_hl(),    // SPHL

            0xEB => self.swap_de_hl(), // XCHG
            0xE3 => self.swap_sp_hl(), // XTHL

            0x87 => self.registers[0] = self.add(self.registers[0], self.registers[0], false), // ADD A
            0x80 => self.registers[0] = self.add(self.registers[0], self.registers[1], false), // ADD B
            0x81 => self.registers[0] = self.add(self.registers[0], self.registers[2], false), // ADD C
            0x82 => self.registers[0] = self.add(self.registers[0], self.registers[3], false), // ADD D
            0x83 => self.registers[0] = self.add(self.registers[0], self.registers[4], false), // ADD E
            0x84 => self.registers[0] = self.add(self.registers[0], self.registers[5], false), // ADD H
            0x85 => self.registers[0] = self.add(self.registers[0], self.registers[6], false), // ADD L
            0x86 => {
                self.registers[0] =
                    self.add(self.registers[0], self.read_byte(self.get_hl()), false)
            } // ADD M
            0xC6 => {
                let byte = self.next_byte();
                self.registers[0] = self.add(self.registers[0], byte, false)
            }

            0x8F => {
                self.registers[0] = self.add(self.registers[0], self.registers[0], self.carry_flag)
            } // ADC A
            0x88 => {
                self.registers[0] = self.add(self.registers[0], self.registers[1], self.carry_flag)
            } // ADC B
            0x89 => {
                self.registers[0] = self.add(self.registers[0], self.registers[2], self.carry_flag)
            } // ADC C
            0x8A => {
                self.registers[0] = self.add(self.registers[0], self.registers[3], self.carry_flag)
            } // ADC D
            0x8B => {
                self.registers[0] = self.add(self.registers[0], self.registers[4], self.carry_flag)
            } // ADC E
            0x8C => {
                self.registers[0] = self.add(self.registers[0], self.registers[5], self.carry_flag)
            } // ADC H
            0x8D => {
                self.registers[0] = self.add(self.registers[0], self.registers[6], self.carry_flag)
            } // ADC L
            0x8E => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.registers[0] = self.add(self.registers[0], byte, self.carry_flag);
            } // ADC M
            0xCE => {
                let byte = self.next_byte();
                self.registers[0] = self.add(self.registers[0], byte, self.carry_flag);
            } // ACI byte

            0x97 => self.registers[0] = self.sub(self.registers[0], self.registers[0], false), // SUB A
            0x90 => self.registers[0] = self.sub(self.registers[0], self.registers[1], false), // SUB B
            0x91 => self.registers[0] = self.sub(self.registers[0], self.registers[2], false), // SUB C
            0x92 => self.registers[0] = self.sub(self.registers[0], self.registers[3], false), // SUB D
            0x93 => self.registers[0] = self.sub(self.registers[0], self.registers[4], false), // SUB E
            0x94 => self.registers[0] = self.sub(self.registers[0], self.registers[5], false), // SUB H
            0x95 => self.registers[0] = self.sub(self.registers[0], self.registers[6], false), // SUB L
            0x96 => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.registers[0] = self.sub(self.registers[0], byte, false);
            } // SUB M
            0xD6 => {
                let byte = self.next_byte();
                self.registers[0] = self.sub(self.registers[0], byte, false);
            } // SUI byte

            0x9F => {
                self.registers[0] = self.sub(self.registers[0], self.registers[0], self.carry_flag)
            } // SBB A
            0x98 => {
                self.registers[0] = self.sub(self.registers[0], self.registers[1], self.carry_flag)
            } // SBB B
            0x99 => {
                self.registers[0] = self.sub(self.registers[0], self.registers[2], self.carry_flag)
            } // SBB C
            0x9A => {
                self.registers[0] = self.sub(self.registers[0], self.registers[3], self.carry_flag)
            } // SBB D
            0x9B => {
                self.registers[0] = self.sub(self.registers[0], self.registers[4], self.carry_flag)
            } // SBB E
            0x9C => {
                self.registers[0] = self.sub(self.registers[0], self.registers[5], self.carry_flag)
            } // SBB H
            0x9D => {
                self.registers[0] = self.sub(self.registers[0], self.registers[6], self.carry_flag)
            } // SBB L
            0x9E => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.registers[0] = self.sub(self.registers[0], byte, self.carry_flag);
            } // SBB M
            0xDE => {
                let byte = self.next_byte();
                self.registers[0] = self.sub(self.registers[0], byte, self.carry_flag);
            } // SBI byte

            0x09 => self.add_word_hl(self.get_bc()), // DAD B
            0x19 => self.add_word_hl(self.get_de()), // DAD D
            0x29 => self.add_word_hl(self.get_hl()), // DAD H
            0x39 => self.add_word_hl(self.stack_pointer), // DAD SP

            0xF3 => self.interupt_flip_flop = false, // DI
            0xFB => {
                self.interupt_flip_flop = true;
                self.interupt_delay = true;
            } // EI
            0x76 => self.halted = true,              // HLT

            0x3C => self.registers[0] = self.inc_byte(self.registers[0]), // INR A
            0x04 => self.registers[1] = self.inc_byte(self.registers[1]), // INR B
            0x0C => self.registers[2] = self.inc_byte(self.registers[2]), // INR C
            0x14 => self.registers[3] = self.inc_byte(self.registers[3]), // INR D
            0x1C => self.registers[4] = self.inc_byte(self.registers[4]), // INR E
            0x24 => self.registers[5] = self.inc_byte(self.registers[5]), // INR H
            0x2C => self.registers[6] = self.inc_byte(self.registers[6]), // INR L
            0x34 => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.inc_byte(byte);
            } // INR M

            0x3D => self.registers[0] = self.dec_byte(self.registers[0]), // DCR A
            0x05 => self.registers[1] = self.dec_byte(self.registers[1]), // DCR B
            0x0D => self.registers[2] = self.dec_byte(self.registers[2]), // DCR C
            0x15 => self.registers[3] = self.dec_byte(self.registers[3]), // DCR D
            0x1D => self.registers[4] = self.dec_byte(self.registers[4]), // DCR E
            0x25 => self.registers[5] = self.dec_byte(self.registers[5]), // DCR H
            0x2D => self.registers[6] = self.dec_byte(self.registers[6]), // DCR L
            0x35 => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.dec_byte(byte);
            } // DCR M
            0x03 => {
                let bc = self.get_bc();
                self.set_bc(bc + 1);
            } // INX B
            0x13 => {
                let de = self.get_de();
                self.set_de(de + 1);
            } // INX D
            0x23 => {
                let hl = self.get_hl();
                self.set_hl(hl + 1);
            } // INX H
            0x33 => self.stack_pointer += 1,                              // INX SP

            0x0B => {
                let bc = self.get_bc();
                self.set_bc(bc - 1);
            } // DCX B
            0x1B => {
                let de = self.get_de();
                self.set_de(de - 1);
            } // DCX D
            0x2B => {
                let hl = self.get_hl();
                self.set_hl(hl - 1);
            } // DCX H
            0x3B => self.stack_pointer -= 1, // DCX SP

            0x27 => self.daa(),                             // DAA
            0x2F => self.registers[0] = !self.registers[0], // CMA
            0x37 => self.carry_flag = true,                 // STC
            0x3F => self.carry_flag = !self.carry_flag,     // CMC

            0x07 => self.rotate_left(),        // RLC
            0x0F => self.rotate_right(),       // RRC
            0x17 => self.rotate_left_carry(),  // RAL
            0x1F => self.rotate_right_carry(), // RAR

            0xA7 => self.and(self.registers[0]), // ANA A
            0xA0 => self.and(self.registers[1]), // ANA B
            0xA1 => self.and(self.registers[2]), // ANA C
            0xA2 => self.and(self.registers[3]), // ANA D
            0xA3 => self.and(self.registers[4]), // ANA E
            0xA4 => self.and(self.registers[5]), // ANA H
            0xA5 => self.and(self.registers[6]), // ANA L
            0xA6 => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.and(byte);
            } // ANA M
            0xE6 => {
                let byte = self.next_byte();
                self.and(byte);
            } // ANI byte

            0xAF => self.xor(self.registers[0]), // XRA A
            0xA8 => self.xor(self.registers[1]), // XRA B
            0xA9 => self.xor(self.registers[2]), // XRA C
            0xAA => self.xor(self.registers[3]), // XRA D
            0xAB => self.xor(self.registers[4]), // XRA E
            0xAC => self.xor(self.registers[5]), // XRA H
            0xAD => self.xor(self.registers[6]), // XRA L
            0xAE => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.xor(byte);
            } // XRA M
            0xEE => {
                let byte = self.next_byte();
                self.xor(byte);
            } // XRI byte

            0xB7 => self.or(self.registers[0]), // ORA A
            0xB0 => self.or(self.registers[1]), // ORA B
            0xB1 => self.or(self.registers[2]), // ORA C
            0xB2 => self.or(self.registers[3]), // ORA D
            0xB3 => self.or(self.registers[4]), // ORA E
            0xB4 => self.or(self.registers[5]), // ORA H
            0xB5 => self.or(self.registers[6]), // ORA L
            0xB6 => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.or(byte);
            } // ORA M
            0xF6 => {
                let byte = self.next_byte();
                self.or(byte);
            } // ORI byte

            0xBF => self.cmp(self.registers[0]), // CMP A
            0xB8 => self.cmp(self.registers[1]), // CMP B
            0xB9 => self.cmp(self.registers[2]), // CMP C
            0xBA => self.cmp(self.registers[3]), // CMP D
            0xBB => self.cmp(self.registers[4]), // CMP E
            0xBC => self.cmp(self.registers[5]), // CMP H
            0xBD => self.cmp(self.registers[6]), // CMP L
            0xBE => {
                let hl = self.get_hl();
                let byte = self.read_byte(hl);
                self.cmp(byte);
            } // CMP M
            0xFE => {
                let byte = self.next_byte();
                self.cmp(byte);
            } // CPI byte

            0xC3 => {
                let address = self.next_word();
                self.jump(address);
            } // JMP
            0xC2 => self.cond_jump(!self.zero_flag),  // JNZ
            0xCA => self.cond_jump(self.zero_flag),   // JZ
            0xD2 => self.cond_jump(!self.carry_flag), // JNC
            0xDA => self.cond_jump(self.carry_flag),  // JC
            0xE2 => self.cond_jump(!self.parity),     // JPO
            0xEA => self.cond_jump(self.parity),      // JPE
            0xF2 => self.cond_jump(!self.sign),       // JP
            0xFA => self.cond_jump(self.sign),        // JM

            0xE9 => self.program_counter = self.get_hl(), // PCHL
            0xCD => {
                let address = self.next_word();
                self.call(address);
            } // CALL

            0xC4 => self.cond_call(!self.zero_flag),  // CNZ
            0xCC => self.cond_call(self.zero_flag),   // CZ
            0xD4 => self.cond_call(!self.carry_flag), // CNC
            0xDC => self.cond_call(self.carry_flag),  // CC
            0xE4 => self.cond_call(!self.parity),     // CPO
            0xEC => self.cond_call(self.parity),      // CPE
            0xF4 => self.cond_call(!self.sign),       // CP
            0xFC => self.cond_call(self.sign),        // CM

            0xC9 => self.returns(),                      // RET
            0xC0 => self.cond_returns(!self.zero_flag),  // RNZ
            0xC8 => self.cond_returns(self.zero_flag),   // RZ
            0xD0 => self.cond_returns(!self.carry_flag), // RNC
            0xD8 => self.cond_returns(self.carry_flag),  // RC
            0xE0 => self.cond_returns(!self.parity),     // RPO
            0xE8 => self.cond_returns(self.parity),      // RPE
            0xF0 => self.cond_returns(!self.sign),       // RP
            0xF8 => self.cond_returns(self.sign),        // RM

            0xC7 => self.call(0x00), // RST 0
            0xCF => self.call(0x08), // RST 1
            0xD7 => self.call(0x10), // RST 2
            0xDF => self.call(0x18), // RST 3
            0xE7 => self.call(0x20), // RST 4
            0xEF => self.call(0x28), // RST 5
            0xF7 => self.call(0x30), // RST 6
            0xFF => self.call(0x38), // RST 7

            0xC5 => self.push_stack(self.get_bc()), // PUSH B
            0xD5 => self.push_stack(self.get_de()), // PUSH D
            0xE5 => self.push_stack(self.get_hl()), // PUSH H
            0xF5 => self.push_a_and_flags(),        // PUSH PSW
            0xC1 => {
                let value = self.pop_stack();
                self.set_bc(value);
            } // POP B
            0xD1 => {
                let value = self.pop_stack();
                self.set_de(value);
            } // POP D
            0xE1 => {
                let value = self.pop_stack();
                self.set_hl(value);
            } // POP H
            0xF1 => self.pop_a_and_flags(),         // POP PSW

            0xDB => {
                self.next_byte();
                self.registers[0] = 0x00;
            } // IN

            0xD3 => {
                let port = self.next_byte();
                self.finished = self.port_out(port);
            } // OUT

            0x08 | 0x10 | 0x18 | 0x20 | 0x28 | 0x30 | 0x38 => {}

            0xD9 => self.returns(), // RET

            0xDD | 0xED | 0xFD => {
                let word = self.next_word();
                self.call(word);
            }

            0xCB => {
                let word = self.next_word();
                self.jump(word);
            }

            _ => panic!("Unknown opcode: {:X}", opcode),
        }
    }

    fn get_hl(&self) -> u16 {
        ((self.registers[5] as u16) << 8) | self.registers[6] as u16
    }

    fn set_hl(&mut self, value: u16) {
        self.registers[5] = (value >> 8) as u8;
        self.registers[6] = value as u8 & 0xFF;
    }

    fn get_de(&self) -> u16 {
        ((self.registers[3] as u16) << 8) | self.registers[4] as u16
    }

    fn set_de(&mut self, value: u16) {
        self.registers[3] = (value >> 8) as u8;
        self.registers[4] = value as u8 & 0xFF;
    }

    fn get_bc(&self) -> u16 {
        ((self.registers[1] as u16) << 8) | self.registers[2] as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.registers[1] = (value >> 8) as u8;
        self.registers[2] = value as u8 & 0xFF;
    }

    fn read_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }

    fn read_word(&self, address: u16) -> u16 {
        let low = self.memory[address as usize];
        let high = self.memory[(address + 1) as usize];
        ((high as u16) << 8) | low as u16
    }

    fn write_word(&mut self, address: u16, value: u16) {
        let low = value as u8;
        let high = (value >> 8) as u8;
        self.memory[address as usize] = low;
        self.memory[(address + 1) as usize] = high;
    }

    fn next_word(&mut self) -> u16 {
        let word = self.read_word(self.program_counter);
        self.program_counter += 2;
        word
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.read_byte(self.program_counter);
        self.program_counter += 1;
        byte
    }

    fn swap_de_hl(&mut self) {
        let de = self.get_de();
        let hl = self.get_hl();
        self.set_de(hl);
        self.set_hl(de);
    }

    fn swap_sp_hl(&mut self) {
        let val = self.read_word(self.stack_pointer);
        let hl = self.get_hl();

        self.write_word(self.stack_pointer, hl);
        self.set_hl(val);
    }

    fn add(&mut self, reg: u8, val: u8, cy: bool) -> u8 {
        let (sum, carry1) = reg.overflowing_add(val);
        let (result, carry2) = sum.overflowing_add(cy as u8);
        self.carry_flag = carry1 || carry2;
        self.half_carry_flag = self.carry(4, reg, val, cy);
        set_zsp!(self, result);

        result
    }

    fn sub(&mut self, reg: u8, val: u8, cy: bool) -> u8 {
        let result = self.add(reg, !val, !cy);
        self.carry_flag = !self.carry_flag;

        result
    }

    fn add_word_hl(&mut self, val: u16) {
        let (result, carry) = self.get_hl().overflowing_add(val);
        self.carry_flag = carry;
        self.set_hl(result);
    }

    fn inc_byte(&mut self, val: u8) -> u8 {
        let result = val + 1;
        self.half_carry_flag = (result & 0xF) == 0;
        set_zsp!(self, result);
        result
    }

    fn dec_byte(&mut self, val: u8) -> u8 {
        let result = val - 1;
        self.half_carry_flag = !(result & 0xF) == 0xF;
        set_zsp!(self, result);
        result
    }

    fn daa(&mut self) {
        let mut cy = self.carry_flag;
        let mut correction = 0;

        let lsb = self.registers[0] & 0x0F;
        let msb = self.registers[0] >> 4;

        if self.half_carry_flag || lsb > 9 {
            correction += 0x06;
        }

        if self.carry_flag || msb > 9 || (msb >= 9 && lsb > 9) {
            correction += 0x60;
            cy = true;
        }

        self.registers[0] = self.add(self.registers[0], correction, false);
        self.carry_flag = cy;
    }

    fn rotate_left(&mut self) {
        self.carry_flag = (self.registers[0] >> 7) != 0;
        self.registers[0] = (self.registers[0] << 1) | self.carry_flag as u8;
    }

    fn rotate_right(&mut self) {
        self.carry_flag = (self.registers[0] & 1) != 0;
        self.registers[0] = (self.registers[0] >> 1) | ((self.carry_flag as u8) << 7);
    }

    fn rotate_left_carry(&mut self) {
        let carry = self.carry_flag;
        self.carry_flag = (self.registers[0] >> 7) != 0;
        self.registers[0] = (self.registers[0] << 1) | carry as u8;
    }

    fn rotate_right_carry(&mut self) {
        let carry = self.carry_flag;
        self.carry_flag = (self.registers[0] & 1) != 0;
        self.registers[0] = (self.registers[0] >> 1) | (carry as u8) << 7;
    }

    fn and(&mut self, val: u8) {
        let result = self.registers[0] & val;
        self.carry_flag = false;
        self.half_carry_flag = ((self.registers[0] | val) & 0x08) != 0;
        set_zsp!(self, result);
        self.registers[0] = result;
    }

    fn or(&mut self, val: u8) {
        self.registers[0] |= val;
        self.carry_flag = false;
        self.half_carry_flag = false;
        set_zsp!(self, self.registers[0]);
    }

    fn xor(&mut self, val: u8) {
        self.registers[0] ^= val;
        self.carry_flag = false;
        self.half_carry_flag = false;
        set_zsp!(self, self.registers[0]);
    }

    fn cmp(&mut self, val: u8) {
        let result = (self.registers[0] as i16) - (val as i16);
        self.carry_flag = (result >> 8) & 1 != 0;
        self.half_carry_flag = !((self.registers[0] ^ result as u8 ^ val) & 0x10) != 0;
        set_zsp!(self, (result as u8) & 0xFF);
    }

    fn jump(&mut self, addr: u16) {
        self.program_counter = addr;
    }

    fn cond_jump(&mut self, cond: bool) {
        let addr = self.next_word();
        if cond {
            self.jump(addr);
        }
    }

    fn call(&mut self, addr: u16) {
        self.push_stack(self.program_counter);
        self.jump(addr);
    }

    fn cond_call(&mut self, cond: bool) {
        let word = self.next_word();
        if cond {
            self.call(word);
        }
    }

    fn push_stack(&mut self, val: u16) {
        self.stack_pointer -= 2;
        self.write_word(self.stack_pointer, val);
    }

    fn returns(&mut self) {
        self.program_counter = self.pop_stack();
    }

    fn cond_returns(&mut self, cond: bool) {
        if cond {
            self.returns();
        }
    }

    fn pop_stack(&mut self) -> u16 {
        let val = self.read_word(self.stack_pointer);
        self.stack_pointer += 2;
        val
    }

    fn push_a_and_flags(&mut self) {
        let mut psw: u8 = 0;
        psw |= (self.sign as u8) << 7;
        psw |= (self.zero_flag as u8) << 6;
        psw |= (self.half_carry_flag as u8) << 4;
        psw |= (self.parity as u8) << 2;
        psw |= 1 << 1; // Always 1
        psw |= (self.carry_flag as u8) << 0;
        self.push_stack((self.registers[0] | psw) as u16);
    }

    fn pop_a_and_flags(&mut self) {
        let af = self.pop_stack();
        self.registers[0] = (af >> 8) as u8;
        let psw = af & 0xFF;

        self.sign = (psw >> 7) != 0;
        self.zero_flag = (psw >> 6) != 0;
        self.half_carry_flag = (psw >> 4) != 0;
        self.parity = (psw >> 2) != 0;
        self.carry_flag = (psw >> 0) != 0;
    }

    fn parity(&self, val: u8) -> bool {
        let mut nb_one_bits = 0;
        for i in 0..8 {
            nb_one_bits += (val >> i) & 1;
        }

        (nb_one_bits & 1) == 0
    }

    fn port_out(&mut self, port: u8) -> bool {
        let mut test_finished = false;

        if port == 0 {
            test_finished = true;
        } else if port == 1 {
            let op = self.registers[2]; // C register

            if op == 2 {
                print!("{}", self.registers[4] as char);
            } else if op == 9 {
                let mut addr = (self.registers[3] as u16) << 8 | self.registers[4] as u16;
                loop {
                    print!("{}", self.read_byte(addr) as char);
                    addr += 1;
                    if self.read_byte(addr) == '$' as u8 {
                        break;
                    }
                }
            }
        }
        test_finished
    }

    fn carry(&self, bit_no: i16, a: u8, b: u8, cy: bool) -> bool {
        let result = (a as u16 + b as u16 + cy as u16) << (8 - bit_no);
        result & 0x100 != 0
    }

    fn port_in(&self) -> u8 {
        0x00
    }

    fn interrupt(&mut self, opcode: u8) {
        self.interrupt_pending = true;
        self.interrupt_vector = opcode
    }
}
