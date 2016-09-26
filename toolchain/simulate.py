import cpu_simulator
import time
import sys
import threading

#import urwid

cpu = None
sleep_time = 0.1

break_lock = threading.Condition()
breaking = False

display_data = [[False for x in range(8)] for y in range(16)]

def bitfield(n):
    return [int(digit) for digit in "{:08b}".format(n)]

class DisplayPeripheral(cpu_simulator.BasePeripheral):
    def write(self, address, value):
        short_address = address & 0b1111
        out = list(map(lambda n: n == 1, bitfield(value)))
        display_data[short_address] = out

class ButtonPeripheral(cpu_simulator.BasePeripheral):
    def __init__(self):
        self.buttons = [False] * 16

    def set_active(self, num):
        self.buttons[num] = True

    def read(self, address):
        short_address = address & 0b111
        state = self.buttons[short_address]
        self.buttons[short_address] = False
        return 1 if state else 0


def set_break(next_breaking):
    breaking = next_breaking
    break_lock.notify_all()

def cpu_worker():
    while True:
        break_lock.wait_for(lambda: not breaking)
        cpu.clock()
        time.sleep(sleep_time)

with open(sys.argv[1], "rb") as f:
    cpu = cpu_simulator.TTLCpu(f.read())

cpu.peripherals[0] = DisplayPeripheral()
cpu.peripherals[1] = ButtonPeripheral()

import curses

class InteractionManager(object):
    def __init__(self, scr):
        self.scr = scr
        self.scr.nodelay(True)
        self.scr.clear()
        self.scr.refresh()

        self.wt = threading.Thread(target=cpu_worker)
        self.wt.daemon = True
        self.wt.start()

    def loop(self):
        try:
            key = self.scr.getch()
            global sleep_time
            if key == curses.KEY_UP:
                sleep_time *= 2
            elif key == curses.KEY_DOWN:
                sleep_time /= 2
            elif key >= 49 and key <= 56:
                num_key = key - 49
                cpu.peripherals[1].set_active(num_key)
        except Exception as e:
            # No input
            pass

        self.draw()

        time.sleep(0.01)

    def draw(self):
        # Draw the screen
        data = list(zip(*display_data))
        for line_num in range(8):
            line_data = data[line_num]
            line = "".join(map(lambda pixel: "\u2588\u2588" if pixel else "\u2591\u2591", line_data))
            self.scr.addstr(line_num+1, 1, line)

        # Draw internal state
        self.scr.addstr(10, 1, "{:04.2f} seconds per instruction".format(sleep_time))
        self.scr.addstr(12, 1, "ACC:{:02x}   Carry:{}".format(cpu.reg_acc.value, cpu.flag_carry.value))
        self.scr.addstr(13, 1, "PC:{:04x}   BSEL:{:02x}   JMPH:{:02x}".format(cpu.reg_pc.value, cpu.reg_bsel.value, cpu.reg_jmp_h.value))
        (last_instr_f, last_instr_l) = cpu.last_instruction
        self.scr.addstr(15, 1, "Instr: {:02x}{:02x} ({:08b} {:08b})".format(last_instr_f, last_instr_l, last_instr_f, last_instr_l))

        self.scr.refresh()

interaction_manager = None

def main(stdscr):
    interaction_manager = InteractionManager(stdscr)
    while True:
        interaction_manager.loop()

curses.wrapper(main)
