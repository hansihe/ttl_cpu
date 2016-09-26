import re

def addr(obj, string):
    if re.match("[iamp].*", string):
        obj["addr"] = string[0]
        string = string[1:]
        return (obj, string)
    else:
        raise Exception("Opcode missing addressing mode")

def cond(obj, string):
    match = re.fullmatch("(t|ez|gz|lz|c)(.*)", string)
    if not match:
        raise Exception("Opcode missing condition")
    obj["cond"] = match.group(1)
    return (obj, match.group(2))

def make_optional(short, name):
    def opt_parser(obj, string):
        if len(string) >= 1 and string[0] == short:
            obj[name] = True
            string = string[1:]
        else:
            obj[name] = False
        return (obj, string)
    return opt_parser

carry = make_optional("c", "carry")
negate = make_optional("n", "negate")

opcode_parsers = [
    ["nop"],
    ["add", carry, addr],
    ["sub", carry, addr],
    ["and", addr],
    ["or", addr],
    ["xor", addr],
    ["not"],
    ["shjmp"],
    ["sbsel"],
    ["lpdbj", negate, cond],
    ["spibj", negate, cond],
    ["l", addr],
    ["s", addr],
    ["j", negate, cond],
]

def parse_opcode(word):
    for opcode_parser in opcode_parsers:
        name = opcode_parser[0]
        aux_pattern = opcode_parser[1:]

        if not word.startswith(name):
            continue
        word = word[len(name):]

        obj = dict(name=name)
        for pattern_part in aux_pattern:
            (obj, word) = pattern_part(obj, word)

        return obj
    return False

