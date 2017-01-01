#define minusOne 255

#define ballX [0]
#define ballY [1]
#define ballDirX [2]
#define ballDirY [3]
#define paddleLY [4]
#define paddleRY [5]

SETUP:

// Initialize paddle positions
mov 4 ballX
mov 4 ballY

// Initialize ball vector
mov minusOne ballDirX
mov minusOne ballDirY

// Initialize paddle positions
mov 3 paddleLY
mov 3 paddleRY

// Make paddle graphics LUT
mov 0b11000000 [0xe0]
mov 0b11100000 [0xe1]
mov 0b01110000 [0xe2]
mov 0b00111000 [0xe3]
mov 0b00011100 [0xe4]
mov 0b00001110 [0xe5]
mov 0b00000111 [0xe6]
mov 0b00000011 [0xe7]

// Make ball graphics LUT
mov 0b10000000 [0xf0]
mov 0b01000000 [0xf1]
mov 0b00100000 [0xf2]
mov 0b00010000 [0xf3]
mov 0b00001000 [0xf4]
mov 0b00000100 [0xf5]
mov 0b00000010 [0xf6]
mov 0b00000001 [0xf7]
mov 0b11000000 [0xe0]

CD:

// Clear display
mov 0 acc
mov acc >[0x00]
mov acc >[0x01]
mov acc >[0x02]
mov acc >[0x03]
mov acc >[0x04]
mov acc >[0x05]
mov acc >[0x06]
mov acc >[0x07]
mov acc >[0x08]
mov acc >[0x09]
mov acc >[0x0a]
mov acc >[0x0b]
mov acc >[0x0c]
mov acc >[0x0d]
mov acc >[0x0e]
mov acc >[0x0f]

MAIN_LOOP:

// === Ball movement ===

// Move ball X
mov ballX acc
add ballDirX acc
mov acc ballX

// Check ball X lower bounds
jnez FINISH_SET_X_DIR_POSITIVE
mov 1 ballDirX
FINISH_SET_X_DIR_POSITIVE:

// Check ball X upper bounds
sub 13 acc
jnez FINISH_SET_X_DIR_NEGATIVE
mov minusOne ballDirX
FINISH_SET_X_DIR_NEGATIVE:

// === Render ball ===
mov ballX acc
add 0xf0 acc // Offset of ball graphics LUT
mov [acc] acc // TODO merge these: mov [acc] %0
mov acc %0
mov ballY acc
add 1 acc
mov acc %1

mov %0 acc // TODO: Merge
mov 0b01010101 acc
//mov acc >[%1]
mov %1 acc
mov 0b10101010 >[acc]

jt CD
