import struct

def bytestr_to_byte(stri):
    out = 0
    for num in range(0, 8):
        if stri[7-num] == "1":
            out = out | 1
        out << 1
    return out

with open("test", "r") as f1:
    with open("testbinary.tb", "wb") as f2:
        while True:
            rin = f1.read(8)
            if len(rin) != 8:
                break
            f2.write(struct.pack("B", bytestr_to_byte(rin)))
