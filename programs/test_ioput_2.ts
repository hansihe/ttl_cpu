#define pos %0

START:

mov 0 pos

LOOP:

add 1 acc
mov acc pos

mov acc >[pos]

jt LOOP
