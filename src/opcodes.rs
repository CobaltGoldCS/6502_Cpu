use crate::{CPU, Mem, CpuFlags};
use crate::AddressingMode;

use std::collections::HashMap;


#[allow(dead_code)]
impl CPU {
        

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let value = self.read(addr);
        self.set_register_a(value);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let value = self.read(addr);
        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let value = self.read(addr);
        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.write(addr, self.register_a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.read(addr);
        self.set_register_a(data & self.register_a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.read(addr);
        self.set_register_a(data ^ self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.read(addr);
        self.set_register_a(data | self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    // Increment X
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    // Increment Y
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.read(addr);
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8)
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let value = self.read(addr);
        self.add_to_register_a(value)
    }

    fn asl_accumulator(&mut self) {
        let mut data = self.register_a;

        self.status.flip_bit(CpuFlags::CARRY,
            data >> 7 == 1);
        data = data << 1;
        self.set_register_a(data)
    }

    fn asl(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);
        self.status.flip_bit(CpuFlags::CARRY, 
            data >> 7 == 1);
        data = data << 1;
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn lsr_accumulator(&mut self) {
        let mut data = self.register_a;
        self.status.flip_bit(CpuFlags::CARRY, 
            data & 1 == 1);
        data = data >> 1;
        self.set_register_a(data)
    }

    fn lsr(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);

        self.status.flip_bit(CpuFlags::CARRY, 
            data & 1 == 1);
        data = data >> 1;
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status.flip_bit(CpuFlags::CARRY, data >> 7 == 1);
        data = data << 1;
        if old_carry {
            data = data | 1;
        }
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn rol_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);
        
        self.status.flip_bit(CpuFlags::CARRY, data >> 7 == 1);
        data = data << 1;
        if old_carry {
            data = data | 1;
        }
        self.set_register_a(data);
    }

    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status.flip_bit(CpuFlags::CARRY, data & 1 == 1);
        data = data >> 1;
        if old_carry {
            data = data | 0b10000000;
        }
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn ror_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status.flip_bit(CpuFlags::CARRY, 
            data & 1 == 1);
        data = data >> 1;
        if old_carry {
            data = data | 0b10000000;
        }
        self.set_register_a(data);
    }

    fn inc(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);
        data = data.wrapping_add(1);
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dec(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.read(addr);
        data = data.wrapping_sub(1);
        self.write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_register_a(data);
    }

    fn plp(&mut self) {
        self.set_flags_using_stack();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    fn php(&mut self) {
        //http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
        let mut flags = self.status.clone();
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.read(addr);
        let and = self.register_a & data;
        if and == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        self.status.set(CpuFlags::NEGATIV, data & 0b10000000 > 0);
        self.status.set(CpuFlags::OVERFLOW, data & 0b01000000 > 0);
    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(mode);
        let data = self.read(addr);
        if data <= compare_with {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump: i8 = self.read(self.program_counter) as i8;
            let jump_addr = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);

            self.program_counter = jump_addr;
        }
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where 
        F: FnMut(&mut CPU),
     {
        let ref opcodes: HashMap<u8, &'static crate::Opcode> = *crate::OPCODES_MAP;

        loop {
            callback(self);
            let code = self.read(self.program_counter);
            self.program_counter += 1;

            let program_counter_state = self.program_counter;

            let opcode = opcodes.get(&code).expect(&format!("Opcode {:x} is not recognized", code));

            match code {

                // LDA
                0xA9 | 0xA5 | 0xAD | 0xB5 | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                    self.lda(&opcode.mode);
                }

                // STA
                0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                0xAA => self.tax(),
                0xE8 => self.inx(),
                0x00 => return,

                /* CLD */ 0xD8 => self.status.remove(CpuFlags::DECIMAL_MODE),
                
                /* CLI */ 0x58 => self.status.remove(CpuFlags::INTERRUPT_DISABLE),
                
                /* CLV */ 0xb8 => self.status.remove(CpuFlags::OVERFLOW),

                /* CLC */ 0x18 => self.status.remove(CpuFlags::CARRY),

                /* SEC */ 0x38 => self.status.insert(CpuFlags::CARRY),

                /* SEI */ 0x78 => self.status.insert(CpuFlags::INTERRUPT_DISABLE),

                /* SED */ 0xf8 => self.status.insert(CpuFlags::DECIMAL_MODE),

                /* PHA */ 0x48 => self.stack_push(self.register_a),

                /* PLA */
                0x68 => {
                    self.pla();
                }

                /* PHP */
                0x08 => {
                    self.php();
                }

                /* PLP */
                0x28 => {
                    self.plp();
                }

                /* ADC */
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.mode);
                }

                /* SBC */
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&opcode.mode);
                }

                /* AND */
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.mode);
                }

                /* EOR */
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&opcode.mode);
                }

                /* ORA */
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&opcode.mode);
                }

                /* LSR */ 0x4a => self.lsr_accumulator(),

                /* LSR */
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&opcode.mode);
                }

                /*ASL*/ 0x0a => self.asl_accumulator(),

                /* ASL */
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&opcode.mode);
                }

                /*ROL*/ 0x2a => self.rol_accumulator(),

                /* ROL */
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.mode);
                }

                /* ROR */ 0x6a => self.ror_accumulator(),

                /* ROR */
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.mode);
                }

                /* INC */
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&opcode.mode);
                }

                /* INY */
                0xc8 => self.iny(),

                /* DEC */
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&opcode.mode);
                }

                /* DEX */
                0xca => self.dex(),
            

                /* DEY */
                0x88 => self.dey(),

                /* CMP */
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.compare(&opcode.mode, self.register_a);
                }

                /* CPY */
                0xc0 | 0xc4 | 0xcc => self.compare(&opcode.mode, self.register_y),

                /* CPX */
                0xe0 | 0xe4 | 0xec => self.compare(&opcode.mode, self.register_x),

                /* JMP Absolute */
                0x4c => {
                    let address = self.read_u16(self.program_counter);
                    self.program_counter = address;
                }

                /* JMP Indirect */
                0x6c => {
                    let address = self.read_u16(self.program_counter);
                    // let indirect_ref = self.read_u16(address);
                    //6502 bug mode with with page boundary:
                    //  if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
                    // the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
                    // i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000

                    let indirect_ref = if address & 0x00FF == 0x00FF {
                        let lo = self.read(address);
                        let hi = self.read(address & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.read_u16(address)
                    };

                    self.program_counter = indirect_ref;
                }

                /* JSR */
                0x20 => {
                    self.stack_push_u16(self.program_counter + 2 - 1);
                    let target_address = self.read_u16(self.program_counter);
                    self.program_counter = target_address
                }

                /* RTS */
                0x60 => {
                    self.program_counter = self.stack_pop_u16() + 1;
                }

                /* RTI */
                0x40 => {
                    self.set_flags_using_stack();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);

                    self.program_counter = self.stack_pop_u16();
                }

                /* BNE */
                0xd0 => {
                    self.branch(!self.status.contains(CpuFlags::ZERO));
                }

                /* BVS */
                0x70 => {
                    self.branch(self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BVC */
                0x50 => {
                    self.branch(!self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BPL */
                0x10 => {
                    self.branch(!self.status.contains(CpuFlags::NEGATIV));
                }

                /* BMI */
                0x30 => {
                    self.branch(self.status.contains(CpuFlags::NEGATIV));
                }

                /* BEQ */
                0xf0 => {
                    self.branch(self.status.contains(CpuFlags::ZERO));
                }

                /* BCS */
                0xb0 => {
                    self.branch(self.status.contains(CpuFlags::CARRY));
                }

                /* BCC */
                0x90 => {
                    self.branch(!self.status.contains(CpuFlags::CARRY));
                }

                /* BIT */
                0x24 | 0x2c => {
                    self.bit(&opcode.mode);
                }

                /* STX */
                0x86 | 0x96 | 0x8e => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.write(addr, self.register_x);
                }

                /* STY */
                0x84 | 0x94 | 0x8c => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.write(addr, self.register_y);
                }

                /* LDX */
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&opcode.mode);
                }

                /* LDY */
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&opcode.mode);
                }

                /* NOP */
                0xea => {
                    //do nothing
                }

                /* TAY */
                0xa8 => {
                    self.register_y = self.register_a;
                    self.update_zero_and_negative_flags(self.register_y);
                }

                /* TSX */
                0xba => {
                    self.register_x = self.stack_pointer;
                    self.update_zero_and_negative_flags(self.register_x);
                }

                /* TXA */
                0x8a => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                }

                /* TXS */
                0x9a => {
                    self.stack_pointer = self.register_x;
                }

                /* TYA */
                0x98 => {
                    self.register_a = self.register_y;
                    self.update_zero_and_negative_flags(self.register_a);
                }
                /* ILLEGAL / UNOFFICIAL OPCODES */


                // ASR 
                0x4B => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let byte = self.read(addr);
                    self.and_with_register_a(byte);
                    self.lsr_accumulator();
                }

                // AAC (ANC) 
                0x0B | 0x2B => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let byte = self.read(addr);
                    self.and_with_register_a(byte);

                    self.status.flip_bit(CpuFlags::CARRY, 
                        self.status.contains(CpuFlags::NEGATIV));
                }

                // AAX (SAX)
                0x87 | 0x97 | 0x83 |0x8F => {
                    let data = self.register_a & self.register_x;
                    let addr = self.get_operand_address(&opcode.mode);
                    self.write(addr, data);
                }

                // ARR
                0x6B => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let byte = self.read(addr);

                    self.and_with_register_a(byte);
                    self.ror_accumulator();
                    let result = self.register_a;
                    let bit_5 = (result >> 5) & 1;
                    let bit_6 = (result >> 6) & 1;

                    self.status.flip_bit(CpuFlags::CARRY, bit_6 == 1);
                    self.status.flip_bit(CpuFlags::OVERFLOW, bit_5 ^ bit_6 == 1);
                    self.update_zero_and_negative_flags(result);
                }

                // LXA
                0xAB => {
                    self.lda(&opcode.mode);
                    self.tax();
                }
                
                /* AXA Absolute Y */
                0x9F => {
                    let mem_address =
                        self.get_operand_address(&opcode.mode);

                    let data = self.register_a & self.register_x & (mem_address >> 8) as u8;
                    self.write(mem_address, data)
                }

                // AXA Indirect Y
                0x93 => {
                    let pos: u8 = self.read(self.program_counter);
                    let mem_addr = self.read_u16(pos as u16) + self.register_y as u16;
                    let data = self.register_a & self.register_x & (mem_addr >> 8) as u8;
                    self.write(mem_addr, data)
                }

                // AXS
                0xCB => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let byte = self.read(addr);

                    let x_and_a = self.register_x & self.register_a;
                    let result = x_and_a.wrapping_sub(byte);

                    if byte <= x_and_a {
                        self.status.insert(CpuFlags::CARRY);
                    }

                    self.update_zero_and_negative_flags(result);
                    self.register_x = result;
                }
                
                // DCP
                0xc7 | 0xd7 | 0xCF | 0xdF | 0xdb | 0xd3 | 0xc3 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let mut byte = self.read(addr);
                    byte = byte.wrapping_sub(1);
                    self.write(addr, byte);

                    if byte <= self.register_a {
                        self.status.insert(CpuFlags::CARRY);
                    }

                    self.update_zero_and_negative_flags(self.register_a.wrapping_sub(byte));
                }

                // DOP (Double NOP)
                0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => { /* do nothing */ }

                // TOP (Triple NOP)
                0x0C | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => { /* Do Nothing */ }
                // KIL (We do not want to actually freeze the processor)
                0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xb2 | 0xd2
                | 0xf2 => { /* do nothing */ }

                // ISC
                0xE7 | 0xF7 | 0xEF | 0xFF | 0xFB | 0xE3 | 0xF3 => {
                    let data = self.inc(&opcode.mode);
                    self.sub_from_register_a(data);
                }

                // LAR (LAS)
                0xBB => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let mut data = self.read(addr);
                    data &= self.stack_pointer;
                    self.register_a = data;
                    self.register_x = data;
                    self.stack_pointer = data;
                    self.update_zero_and_negative_flags(data);
                }

                // LAX
                0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let byte = self.read(addr);
                    self.set_register_a(byte);
                    self.register_x = self.register_a;
                }

                // RLA
                0x27 | 0x37 | 0x2F | 0x3F | 0x3B | 0x23 | 0x33 => {
                    let byte = self.rol(&opcode.mode);
                    self.and_with_register_a(byte);
                }

                // RRA
                0x67 | 0x77 | 0x6F | 0x7F | 0x7B | 0x63 | 0x73 => {
                    let byte = self.ror(&opcode.mode);
                    self.and_with_register_a(byte);
                }
                // Unofficial SBC
                0xeb => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let data = self.read(addr);
                    self.sub_from_register_a(data);
                }
                
                // SLO (TODO: Tests)
                0x07 | 0x17 | 0x0F | 0x1F | 0x1B | 0x03 | 0x13 => {
                    let byte = self.asl(&opcode.mode);
                    self.or_with_register_a(byte);
                }

                // SRE (TODO: Tests)
                0x47 | 0x57 | 0x4F | 0x5F | 0x5B | 0x43 | 0x53 => {
                    let byte = self.lsr(&opcode.mode);
                    self.xor_with_register_a(byte);
                }

                /* SKB */
                0x80 | 0x82 | 0x89 | 0xc2 | 0xe2 => {
                    /* 2 byte NOP (immidiate ) */
                    // todo: might be worth doing the read
                }
                
                // SHX
                0x9E => {
                    let word = 
                        self.read_u16(self.program_counter) + self.register_y as u16;
                    // todo if cross page boundry {
                    //     mem_address &= (self.x as u16) << 8;
                    // }
                    let data = self.register_x & ((word >> 8) as u8 + 1);
                    self.write(word, data);
                }

                // SHY
                0x9C => {
                    let word = 
                        self.read_u16(self.program_counter) + self.register_x as u16;
                    // todo if cross page boundry {
                    //     mem_address &= (self.x as u16) << 8;
                    // }
                    let data = self.register_y & ((word >> 8) as u8 + 1);
                    self.write(word, data);
                }
                
                /* XAA */
                0x8B => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                    let addr = self.get_operand_address(&opcode.mode);
                    let data = self.read(addr);
                    self.and_with_register_a(data);
                }

                /* XAS */
                0x9B => {
                    let data = self.register_x & self.register_a;
                    self.stack_pointer = data;
                    let addr = self.get_operand_address(&opcode.mode);
                    let data = 
                        ((self.read_u16(addr) >> 8) + 1) as u8 & self.stack_pointer;
                    self.write(addr, data);
                }
                

                _ => todo!(),
            }
            
            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16
            }
        }
    }
}