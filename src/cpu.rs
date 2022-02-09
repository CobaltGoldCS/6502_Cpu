use bitflags::bitflags;
use crate::Mem;
use crate::Bus;

const STACK_RESET: u8 = 0xfd;
const STACK: u16 = 0x0100;
const PRG_START: u16 = 0x0600;

bitflags! {
    /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
    ///
    ///  7 6 5 4 3 2 1 0
    ///  N V _ B D I Z C
    ///  | |   | | | | +--- Carry Flag
    ///  | |   | | | +----- Zero Flag
    ///  | |   | | +------- Interrupt Disable
    ///  | |   | +--------- Decimal Mode (not used on NES)
    ///  | |   +----------- Break Command
    ///  | +--------------- Overflow Flag
    ///  +----------------- Negative Flag
    ///
    pub struct CpuFlags: u8 {
        const CARRY             = 1 << 0;
        const ZERO              = 1 << 1;
        const INTERRUPT_DISABLE = 1 << 2;
        const DECIMAL_MODE      = 1 << 3;
        const BREAK             = 1 << 4;
        const BREAK2            = 1 << 5;
        const OVERFLOW          = 1 << 6;
        const NEGATIV           = 1 << 7;
    }
}

impl CpuFlags {
    pub fn flip_bit(mut self, flag: CpuFlags, on: bool) {
        if on {
            self |= flag
        }
        else {
            self &= !flag
        }
    }
}


pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus,
 }

 impl Mem for CPU {
    fn read(&self, addr: u16) -> u8 {
        self.bus.read(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.bus.write(addr, data)
    }
 }
 #[allow(dead_code)]
 impl CPU {

    pub fn new(bus: Bus) -> CPU {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            stack_pointer: STACK_RESET,
            program_counter: 0,
            bus: bus
        }
    }

    pub fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.read((STACK as u16) + self.stack_pointer as u16)
    }

    pub fn stack_push(&mut self, data: u8) {
        self.write((STACK as u16) + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    pub fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    pub fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    pub fn set_flags_using_stack(&mut self) {
        self.status.bits = self.stack_pop();
    }

     pub fn update_zero_and_negative_flags(&mut self, value: u8) {
        self.status.flip_bit(CpuFlags::ZERO, 
            value == 0);
        self.status.flip_bit(CpuFlags::NEGATIV,
            value & 0b1000_0000 != 0);
     }

    pub fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16
            + data as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        // Carry when sum is greater than what a byte can hold
        let carry = sum > 0xff;

        self.status.flip_bit(CpuFlags::CARRY, carry);

        let result = sum as u8;
        
        // Check if a result overflows to become negative?
        self.status.flip_bit(CpuFlags::OVERFLOW, 
            (data ^ result) & (result ^ self.register_a) & 0x80 != 0);

        self.set_register_a(result);
    }

    pub fn sub_from_register_a(&mut self, data: u8) {
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    pub fn and_with_register_a(&mut self, data: u8) {
        self.set_register_a(data & self.register_a);
    }

    pub fn xor_with_register_a(&mut self, data: u8) {
        self.set_register_a(data ^ self.register_a);
    }

    pub fn or_with_register_a(&mut self, data: u8) {
        self.set_register_a(data | self.register_a);
    }

    pub fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(value)
    }
 
 
     pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.write(PRG_START + i, program[i as usize]);
        }
     }

     pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);
 
        // KEEP AN EYE ON THIS *VERY IMPORTANT (this is different from the source)
        self.program_counter = PRG_START;
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_|{});
    }
 }