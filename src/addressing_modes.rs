use crate::CPU;
use crate::Mem;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
   Immediate,
   ZeroPage,
   ZeroPage_X,
   ZeroPage_Y,
   Absolute,
   Absolute_X,
   Absolute_Y,
   Indirect_X,
   Indirect_Y,
   NoneAddressing,
}


impl CPU {
    pub fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
 
        match mode {
            // Address is the program counter
            AddressingMode::Immediate => self.program_counter,

            // Address is read using the program counter (first 256)
            AddressingMode::ZeroPage  => self.read(self.program_counter) as u16,
            // Address is read using the program counter 
            AddressingMode::Absolute => self.read_u16(self.program_counter),
            
            // Zero page, offset using x register
            AddressingMode::ZeroPage_X => {
                let pos = self.read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            // Zero page, offset using y register
            AddressingMode::ZeroPage_Y => {
                let pos = self.read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            // Full memory location access, offset using x register
            AddressingMode::Absolute_X => {
                let base = self.read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            // Full memory location access, offset using y register
            AddressingMode::Absolute_Y => {
                let base = self.read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            // Create pointers using x register
            AddressingMode::Indirect_X => {
                let base = self.read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.read(ptr as u16);
                let hi = self.read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            // Create pointers using x register
            AddressingMode::Indirect_Y => {
                let base = self.read(self.program_counter);

                let lo = self.read(base as u16);
                let hi = self.read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
           
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
 
    }
    
    pub fn get_absolute_address(&self, mode: &AddressingMode, addr: u16) -> u16 {
        match mode {
            AddressingMode::ZeroPage => self.read(addr) as u16,

            AddressingMode::Absolute => self.read_u16(addr),

            AddressingMode::ZeroPage_X => {
                let pos = self.read(addr);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.read(addr);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.read_u16(addr);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.read_u16(addr);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.read(addr);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.read(ptr as u16);
                let hi = self.read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.read(addr);

                let lo = self.read(base as u16);
                let hi = self.read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }

            _ => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }
}