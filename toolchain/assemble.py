import sys
import asm_grammar
import io
import re
import struct
import math
import traceback

def parse_literal(text):
    if text.startswith("0b"):
        return (True, int(text[2:], 2))
    elif text.startswith("0x"):
        return (True, int(text[2:], 16))
    else:
        try:
            return (True, int(text, 10))
        except ValueError:
            return (False, text)

def parse_line(line):
    words = line.strip().lower().split(" ")
    words_num = len(words)
    if words_num < 1:
        return False
    try:
        parsed = asm_grammar.parse_opcode(words[0])
    except Exception as e:
        print("Error in line '%s'" % line)
        traceback.print_exc()
        raise e
    if not parsed:
        return False
    if words_num >= 2 and not words[1].startswith("#"):
        parsed["literal"] = parse_literal(words[1])
    return parsed

def address_mode_mask(mode):
    if mode == "i":
        return 0b000
    elif mode == "a":
        return 0b001
    elif mode == "m":
        return 0b010
    elif mode == "p":
        return 0b011
    else:
        raise Exception("bug!")

def cond_mask(cond):
    if cond == "t":
        return 0b000
    elif cond == "ez":
        return 0b001
    elif cond == "gz":
        return 0b010
    elif cond == "lz":
        return 0b011
    elif cond == "c":
        return 0b100
    else:
        raise Exception("bug!")

base_opcodes = {
    "nop": 0b00000000,
    "l": 0b00001000,
    "s": 0b00010000,
    "add": 0b00100000,
    "sub": 0b00110000,
    "and": 0b01000000,
    "or": 0b01001000,
    "xor": 0b01010000,
    "not": 0b01011000,
    "shjmp": 0b01100000,
    "sbsel": 0b01101000,
    "lpdbj": 0b10010000,
    "spibj": 0b10100000,
    "j": 0b10110000,
}

def assemble_instruction(source):
    source = source.strip()

    if source.startswith("#") or source == "":
        return bytes()

    label_match = re.match(r"^([\w\d]+):", source)
    if label_match:
        labels[label_match.group(1).lower()] = math.floor(out_binary.tell()/2)
        return bytes()

    assign_match = re.match(r"^([\w\d]+)\s+=\s+([\w\d]+)", source)
    if assign_match:
        name = assign_match.group(1)
        num = assign_match.group(2)
        (is_num, num) = parse_literal(num)
        if not is_num:
            raise Exception("Define must be a number")
        labels[name.lower()] = num
        return bytes()

    parsed = parse_line(source)
    if not parsed:
        print("ERROR:", source)
        raise Exception("Parse error")

    name = parsed["name"]
    base = base_opcodes[name]

    aux_num = 0
    if "literal" in parsed:
        (aux_is_num, aux) = parsed["literal"]
        if aux_is_num:
            aux_num = aux
        else:
            fill_in.append((out_binary.tell()+1, aux))

    if "addr" in parsed:
        base |= address_mode_mask(parsed["addr"])
    if "cond" in parsed:
        base |= cond_mask(parsed["cond"])
    if "carry" in parsed and parsed["carry"]:
        base |= 0b00001000
    if "negate" in parsed and parsed["negate"]:
        base |= 0b00001000

    parsed["op"] = "{:02x}".format(base)
    print(parsed)

    return bytes([base, aux_num])

with open(sys.argv[1]) as f:
    source = f.read()
out_binary = io.BytesIO()

fill_in = []
labels = dict()

lines = source.split("\n")
for line in lines:
    l = assemble_instruction(line)
    out_binary.write(l)
print(labels)

for (location, label) in fill_in:
    out_binary.seek(location)
    out_binary.write(bytes([labels[label]]))

with open(sys.argv[2], "wb") as f1:
    f1.write(out_binary.getvalue())
