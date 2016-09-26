import struct

INSTR_5_MASK = ((1 << 5) - 1) << 3
INSTR_4_MASK = ((1 << 4) - 1) << 4

BYTE_MASK = (1 << 8) - 1
BYTE_OVERFLOW_MASK = ~((1 << 8) - 1)

class BasePeripheral(object):
    def read(self, address):
        return 0
    def write(self, address, value):
        pass

class Register(object):
    def __init__(self, width):
        self.width = width
        self.bit_mask = (1 << width) - 1
        self._value = 0

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        self._value = value & self.bit_mask

class TTLCpu(object):
    def __init__(self, instruction_rom):
        self.peripherals = [None] * 16

        self.irom = instruction_rom
        self.ram = bytearray(255)

        self.reg_pc = Register(16)
        self.reg_acc = Register(8)
        self.flag_carry = Register(1)

        self.reg_jmp_h = Register(8)
        self.reg_bsel = Register(8)

        self.last_instruction = (0, 0)

    def is_regmem(self, address):
        return (address & 0b11110000) == 0
    def is_iomem(self, address):
        return (self.reg_bsel.value & 0b11111000) == 0b11111000

    def m_read(self, address):
        full_address = (self.reg_bsel.value << 8) | address
        if self.is_regmem(address):
            return self.ram[address]
        elif self.is_iomem(address):
            peripheral = self.peripherals[self.reg_bsel.value & 0b111]
            if peripheral:
                return peripheral.read(address)
            else:
                return 0
        else:
            return self.ram[full_address]

    def m_write(self, address, value):
        full_address = (self.reg_bsel.value << 8) | address
        if self.is_regmem(address):
            self.ram[address] = value
        elif self.is_iomem(address):
            peripheral = self.peripherals[self.reg_bsel.value & 0b111]
            if peripheral:
                peripheral.write(address, value)
        else:
            self.ram[full_address] = value

    def am_set(self, opcode, value, instr_aux):
        mode = opcode & 0b111
        if mode == 0:
            # Do something strange here
            pass
        elif mode == 1:
            # WTF
            pass
        elif mode == 2:
            self.m_write(instr_aux, value)
        elif mode == 3:
            self.m_write(self.m_read(instr_aux), value)

    def am_get(self, opcode, instr_aux):
        mode = opcode & 0b111
        if mode == 0:
            return instr_aux
        elif mode == 1:
            return self.reg_acc.value
        elif mode == 2:
            #print("GET", instr_aux, self.m_read(instr_aux))
            return self.m_read(instr_aux)
        elif mode == 3:
            return self.m_read(self.m_read(instr_aux))
        return 0

    def eval_cond(self, opcode):
        mode = opcode & 0b111
        value = False

        Z = self.reg_acc.value == 0
        S = (self.reg_acc.value & (1 << 7)) != 0

        if mode == 0:
            value = True
        elif mode == 1:
            value = Z
        elif mode == 2:
            value = not S and not Z
        elif mode == 3:
            value = S
        elif mode == 4:
            value = self.flag_carry.value == 1

        if (opcode & (1 << 3)) != 0:
            value = not value

        return value

    def clock(self):
        true_pc = self.reg_pc.value * 2
        (instr, instr_aux) = struct.unpack("BB", self.irom[true_pc:true_pc+2])
        self.last_instruction = (instr, instr_aux)
        self.reg_pc.value += 1

        instr_5 = instr & INSTR_5_MASK
        instr_4 = instr & INSTR_4_MASK

        if instr == 0b00000000: # NOP
            pass
        elif instr_5 == 0b00001000: # La
            self.reg_acc.value = self.am_get(instr, instr_aux)
        elif instr_5 == 0b00010000: # Sa
            self.am_set(instr, self.reg_acc.value, instr_aux)
        elif instr_4 == 0b00100000: # ADD(C)a
            carry = self.flag_carry.value if (instr & (1 << 3)) != 0 else 0
            i_val = self.reg_acc.value + self.am_get(instr, instr_aux) + carry
            if (i_val & BYTE_OVERFLOW_MASK) == 0:
                self.flag_carry.value = 0
            else:
                self.flag_carry.value = 1
            self.reg_acc.value = i_val
        elif instr_4 == 0b00110000: # SUB(C)a
            carry = self.flag_carry.value if (instr & (1 << 3)) != 0 else 0
            # Two's compliment subtract
            i_val = self.reg_acc.value + ((~self.am_get(instr, instr_aux)) & 0xff) + ((~carry) & 0b1)
            if (i_val & BYTE_OVERFLOW_MASK) == 0:
                self.flag_carry.value = 0
            else:
                self.flag_carry.value = 1
            self.reg_acc.value = i_val
        elif instr_5 == 0b01000000: # ANDa
            self.reg_acc.value = self.reg_acc.value & self.am_get(instr, instr_aux)
        elif instr_5 == 0b01001000: # ORa
            self.reg_acc.value = self.reg_acc.value | self.am_get(instr, instr_aux)
        elif instr_5 == 0b01010000: # XORa
            self.reg_acc.value = self.reg_acc.value ^ self.am_get(instr, instr_aux)
        elif instr_5 == 0b01011000: # NOT
            self.reg_acc.value = ~self.reg_acc.value
        elif instr_5 == 0b01100000: # SHJMP
            self.reg_jmp_h.value = self.am_get(instr, instr_aux)
        elif instr_5 == 0b01101000: # SBSEL
            self.reg_bsel.value = self.am_get(instr, instr_aux)
        elif instr_4 == 0b10010000: # LPDBJ(N)cc
            # TODO
            if self.eval_cond(instr):
                pass
        elif instr_4 == 0b10010000: # SPIBJ(N)cc
            # TODO
            if self.eval_cond(instr):
                pass
        elif instr_4 == 0b10110000: # J(N)cc
            if self.eval_cond(instr):
                self.reg_pc.value = ((self.reg_jmp_h.value & BYTE_MASK) << 8) | instr_aux
