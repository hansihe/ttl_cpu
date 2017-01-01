START:
mov 0 >[0]
mov 0 >[1]
mov 0 >[2]
mov 0 >[3]

mov 0 acc
mov 0b00000001 >[acc]
mov 0b00000010 >[acc]
mov 0b00000100 >[acc]
mov 0b00001000 >[acc]

mov 1 %0
mov 0b00000001 acc
mov acc >[%0]
mov 0b00000010 acc
mov acc >[%0]
mov 0b00000100 acc
mov acc >[%0]
mov 0b00001000 acc
mov acc >[%0]

mov 0b00000001 %0
mov 0b00000010 %1
mov 0b00000100 %2
mov 0b00001000 %3
mov %0 >[2]
mov %1 >[2]
mov %2 >[2]
mov %3 >[2]

mov 0b00000001 acc
mov acc >[3]
mov 0b00000010 acc
mov acc >[3]
mov 0b00000100 acc
mov acc >[3]
mov 0b00001000 acc
mov acc >[3]

jt START
