import sys

#with open(sys.argv[1], "r") as f:
#    source = f.read()
from subprocess import Popen, PIPE

process = Popen(["m4", sys.argv[1]], stdout=PIPE)
(source, err) = process.communicate()
assert process.returncode == 0

# First pass parser
lines = source.split("\n")
directives = []
curr_directive = None
for line in lines:
    line = line.strip()
    if line.startswith("#") or line == "":
        continue
    if line.startswith(":"):
        if curr_directive:
            directives.append(curr_directive)
        curr_directive = dict()
        args = line.split(" ")
        curr_directive["directive"] = args[0][1:]
        curr_directive["args"] = args[1:]
        curr_directive["items"] = []
        continue
    if not curr_directive:
        raise Exception("items outside of directive")

    curr_directive["items"].append(line)

if curr_directive:
    directives.append(curr_directive)

# Handle directives
default_opcode = None
def handle_default_directive(descr):
    global default_opcode
    default_opcode = parse_full_opcode(descr["items"])

stage_overrides = [None]*16
def handle_override_pcs_directive(descr):
    stage = dict()
    stage["num"] = int(descr["args"][0], 16)
    stage["ops"] = []
    for item in descr["items"]:
        stage["ops"].append(item)
    stage_overrides[stage["num"]] = stage

opcodes = dict()
def handle_opcode_directive(descr):
    opcode = int(descr["args"][0], 16)
    stages = parse_full_opcode(descr["items"])
    opcodes[opcode] = dict(opcode=opcode, stages=stages)

def parse_full_opcode(items):
    stages = [None]*16
    curr_stage = None
    for item in items:
        item = item.strip()
        if item.startswith("#") or item == "":
            continue
        if item.startswith("<"):
            if curr_stage:
                stages[curr_stage["num"]] = curr_stage
            curr_stage = dict()
            curr_stage["num"] = int(item[1:], 16)
            curr_stage["ops"] = []
            continue
        if not curr_stage:
            raise Exception("instr outside stage")
        curr_stage["ops"].append(item)
    if curr_stage:
        stages[curr_stage["num"]] = curr_stage
    return stages

directive_handlers = dict(
    default=handle_default_directive,
    opcode=handle_opcode_directive,
    override_pcs=handle_override_pcs_directive
)

for directive in directives:
    directive_handler = directive_handlers[directive["directive"].lower()]
    directive_handler(directive)

def merge_override_stages(opcode_descr, overrides):
    out = []
    for (op, op_override) in zip(opcode_descr, overrides):
        if op_override:
            out.append(op_override)
        else:
            out.append(op)
    return out

for (key, val) in opcodes.items():
    val["stages"] = merge_override_stages(val["stages"], stage_overrides)
if default_opcode:
    default_opcode = merge_override_stages(default_opcode, stage_overrides)

ops = dict(
    latch_acc = (0, 0, 0, 0b00000001),
    latch_pc = (0, 0, 0, 0b00000010),
    latch_pc_hs = (0, 0, 0, 0b00000100),
    latch_ir = (0, 0, 0, 0b00001000),
    latch_aluo = (0, 0, 0, 0b00010000),
    latch_carry = (0, 0, 0, 0b00100000),
    latch_ra_l = (0, 0, 0, 0b01000000),
    latch_ra_h = (0, 0, 0, 0b10000000),
    latch_rm = (0, 0, 0b00000001, 0),
    emit_dbus_pca = (0, 0, 0b00000010, 0),
    emit_pc_pca = (0, 0, 0b00000100, 0),
    emit_pch_dbus = (0, 0, 0b00001000, 0),
    emit_pcl_dbus = (0, 0, 0b00010000, 0),
    emit_aluo_dbus = (0, 0, 0b00100000, 0),
    emit_aluo_reg_dbus = (0, 0, 0b01000000, 0),
    emit_irh_dbus = (0, 0, 0b10000000, 0),
    emit_rm_dbus = (0, 0b00000001, 0, 0),
    emit_ra_h_dbus = (0, 0b00000010, 0, 0),
    alu_add = (0b00000000, 0, 0, 0),
    alu_and = (0b00000001, 0, 0, 0),
    alu_or = (0b00000010, 0, 0, 0),
    alu_xor = (0b00000011, 0, 0, 0),
    alu_pass_a = (0b00000100, 0, 0, 0),
    alu_pass_b = (0b00001000, 0, 0, 0),
    alu_pass_cin = (0b00010000, 0, 0, 0),
    alu_inv_a = (0b00100000, 0, 0, 0),
    alu_inv_b = (0b01000000, 0, 0, 0),
    alu_inv_cin = (0b10000000, 0, 0, 0),
)

NEXT_C_MASK = (0, 0b11110000, 0, 0)
ALU_MODE_MASK = (0b11111111, 0, 0, 0)

def parse_selector(selector):
    inv = True
    (zero, sign, carry) = (None, None, None)
    for char in selector[1:]:
        if char == "!":
            inv = not inv
        elif char == "Z":
            zero = inv
            inv = True
        elif char == "S":
            sign = inv
            inv = True
        elif char == "C":
            carry = inv
            inv = True
        else:
            raise Exception("Unknown conditional '{}'".format(char))
    return (zero, sign, carry)
def apply_stage_op(mc, op):
    parts = op.split(" ")
    (zero, sign, carry) = (None, None, None)
    if parts[0].startswith("@"):
        (zero, sign, carry) = parse_selector(parts[0])
        action = parts[1]
        rest = parts[2:]
    else:
        action = parts[0]
        rest = parts[1:]

    for carry_f in [0, 1]:
        if not ((bool(carry_f) != carry) or (carry is None)):
            continue
        for sign_f in [0, 1]:
            if not ((bool(sign_f) != sign) or (sign is None)):
                continue
            for zero_f in [0, 1]:
                if not ((bool(zero_f) != zero) or (zero is None)):
                    continue
                num = (carry_f << 2) | (sign_f << 1) | zero_f
                idx = num * 4
                action = action.lower()

                if action.startswith(">"):
                    num = int(action[1:], 16)
                    mc[idx+1] |= (num & 0b1111) << 4
                elif action == "alu_mode":
                    mode_name = rest[0].lower()
                    mode_actions = alu_modes[mode_name]
                    for mode_action in mode_actions:
                        (b0, b1, b2, b3) = ops[mode_action]
                        mc[idx+0] |= b0
                        mc[idx+1] |= b1
                        mc[idx+2] |= b2
                        mc[idx+3] |= b3
                else:
                    (b0, b1, b2, b3) = ops[action]
                    mc[idx+0] |= b0
                    mc[idx+1] |= b1
                    mc[idx+2] |= b2
                    mc[idx+3] |= b3

alu_modes = {
    "short_a": ["alu_pass_a", "alu_add"],
    "short_b": ["alu_pass_b", "alu_add"],
    "add": ["alu_pass_a", "alu_pass_b", "alu_add"],
    "addc": ["alu_pass_a", "alu_pass_b", "alu_pass_cin", "alu_add"],
    "sub_a": ["alu_pass_a", "alu_pass_b", "alu_inv_a", "alu_inv_cin", "alu_add"],
    "subc_a": ["alu_pass_a", "alu_pass_b", "alu_inv_a", "alu_pass_cin", "alu_inv_cin", "alu_add"],
    "sub_b": ["alu_pass_a", "alu_pass_b", "alu_inv_b", "alu_inv_cin", "alu_add"],
    "subc_b": ["alu_pass_a", "alu_pass_b", "alu_inv_b", "alu_pass_cin", "alu_inv_cin", "alu_add"],
    "not_a": ["alu_pass_a", "alu_inv_a", "alu_add"],
    "not_b": ["alu_pass_b", "alu_inv_b", "alu_add"],
    "and": ["alu_pass_a", "alu_pass_b", "alu_and"],
    "or": ["alu_pass_a", "alu_pass_b", "alu_or"],
    "xor": ["alu_pass_a", "alu_pass_b", "alu_xor"],
}

def compile_stage(stage):
    mc = bytearray(16*4)
    for op in stage["ops"]:
        apply_stage_op(mc, op)
    return mc

def compile_opcode(num, opcode):
    result = bytes()
    for stage in opcode:
        #print("Stage: ", stage)
        if stage:
            result += compile_stage(stage)
        else:
            result += bytearray(16*4)
    return result

print(default_opcode)

result_bin = bytes()
for opcode_num in range(0, 256):
    opcode = None
    if opcode_num in opcodes:
        opcode = opcodes.get(opcode_num)["stages"]
    else:
        opcode = default_opcode

    if opcode:
        result_bin += compile_opcode(opcode_num, opcode)
    else:
        result_bin += bytearray(256*4)

#import pprint
#pprint.pprint(opcodes)

with open(sys.argv[2], "wb") as f:
    f.write(result_bin)
