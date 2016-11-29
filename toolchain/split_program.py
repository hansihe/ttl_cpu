import sys

with open(sys.argv[1], 'rb') as i:
    f1 = open(sys.argv[1] + ".0", 'wb')
    f2 = open(sys.argv[1] + ".1", 'wb')
    while True:
        data = i.read(2)
        if len(data) == 0:
            break
        elif len(data) == 1:
            raise Exception("Odd number of bytes in input file")
        f1.write(bytes([data[0]]))
        f2.write(bytes([data[1]]))
    f1.close()
    f2.close()
