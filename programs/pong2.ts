#define minusOne 255

#define mainBank 0b00000000
#define displayBank 0b11111000
#define buttonBank 0b11111001

#define ballX [0x10]
#define ballY [0x11]
#define ballDirX [0x12]
#define ballDirY [0x13]

#define paddleLY [0x14]
#define paddleRY [0x15]

SETUP:
// Initialize ball position
sbsel mainBank
mov 4 acc
mov acc ballX
mov acc ballY

// Initialize ball vector
mov minusOne acc
mov acc ballDirX
mov acc ballDirY

// Initialize paddle positions
mov 3 acc
mov acc paddleLY
mov acc paddleRY

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

sbsel displayBank
// This is both faster and conserves instruction memory
mov 0 acc
mov acc [0x10]
mov acc [0x11]
mov acc [0x12]
mov acc [0x13]
mov acc [0x14]
mov acc [0x15]
mov acc [0x16]
mov acc [0x17]
mov acc [0x18]
mov acc [0x19]
mov acc [0x1a]
mov acc [0x1b]
mov acc [0x1c]
mov acc [0x1d]
mov acc [0x1e]
mov acc [0x1f]
sbsel mainBank

MAIN_LOOP:

// === Paddle movement ===
// Get buttons
sbsel buttonBank
mov [0x10] %0
mov [0x11] %1
mov [0x12] %2
mov [0x13] %3
sbsel mainBank

// Apply paddle left
mov %0 acc
jez SKIP_B1
mov paddleLY acc
add 1 acc
mov acc paddleLY
SKIP_B1:
mov %1 acc
jez SKIP_B2
mov paddleLY acc
sub 1 acc
mov acc paddleLY
SKIP_B2:

// Apply paddle right
mov %2 acc
jez SKIP_B3
mov paddleRY acc
add 1 acc
mov acc paddleRY
SKIP_B3:
mov %3 acc
jez SKIP_B4
mov paddleRY acc
sub 1 acc
mov acc paddleRY
SKIP_B4:

// Check paddle left bounds
// Lower
mov paddleLY acc
jgez SKIP_L_CLAMP_1
mov 0 paddleLY
SKIP_L_CLAMP_1:
// Upper
mov paddleLY acc
sub 7 acc
jlz SKIP_L_CLAMP_2
mov 7 acc
mov acc paddleLY
SKIP_L_CLAMP_2:

// Check paddle right bounds
// Lower
mov paddleRY acc
jgez SKIP_R_CLAMP_1
mov 0 paddleRY
SKIP_R_CLAMP_1:
// Upper
mov paddleRY acc
sub 7 acc
jlz SKIP_R_CLAMP_2
mov 7 acc
mov acc paddleRY
SKIP_R_CLAMP_2:

// === Ball movement ===
// Move ball X
mov ballX acc
add ballDirX acc
mov acc ballX

// Check ball lower bounds
mov ballX acc
jnez FINISH_SET_X_DIR_POSITIVE
mov 1 ballDirX
FINISH_SET_X_DIR_POSITIVE:

// Check ball upper bounds
mov ballX acc
sub 7 acc
jnez FINISH_SET_X_DIR_NEGATIVE
mov minusOne ballDirX
FINISH_SET_X_DIR_NEGATIVE:

// Move ball Y
mov ballY acc
add ballDirY acc
mov acc ballY

// Check ball lower bounds
mov ballY acc
jnez FINISH_SET_Y_DIR_POSITIVE
mov 1 ballDirY
FINISH_SET_Y_DIR_POSITIVE:

// Check ball upper bounds
mov ballY acc
sub 13 acc
jnez FINISH_SET_Y_DIR_NEGATIVE
mov minusOne ballDirY
FINISH_SET_Y_DIR_NEGATIVE:

// === Render ball ===
// Fetch ball graphic
mov ballX acc
add 0xf0 acc // Offset to start of ball graphics LUT
mov [acc] acc // Fetch value from LUT
mov acc %0
mov ballY acc
add 1 acc // Offset 1 from edge to make room for paddle
add 0xa0 acc // Offset to start of graphics memory
mov acc %1
mov %0 acc
sbsel displayBank
mov acc [%1] // Store graphic to graphics memory

// Clear neighbouring lines
mov %1 acc
sub 1 acc
mov acc %1
mov 0 acc
mov acc [%1]
mov %1 acc
add 2 acc
mov acc %1
mov 0 acc
mov acc [%1]
sbsel mainBank

// === Render paddles ===
// Fetch left paddle graphic
mov paddleLY acc
add 0xe0 acc
mov [acc] acc
mov acc %1

// Fetch right paddle graphic
mov paddleRY acc
add 0xe0 acc
mov acc %0
mov [acc] acc
mov acc %2

sbsel displayBank
mov %1 [0x10]
mov %2 [0x1f]
sbsel mainBank

// === Check for loss ===
// Left
mov ballY acc
sub 0 acc
jnez LEFT_NO_HIT
mov ballX acc
sub paddleLY acc
sub 1 acc
jgz PLAYER_L_LOSS
add 2 acc
jlz PLAYER_L_LOSS
LEFT_NO_HIT:

// Right
mov ballY acc
sub 13 acc
jnez RIGHT_NO_HIT
mov ballX acc
sub paddleRY acc
sub 1 acc
jgz PLAYER_R_LOSS
add 2 acc
jlz PLAYER_R_LOSS
RIGHT_NO_HIT:

jt MAIN_LOOP

PLAYER_R_LOSS:
sbsel displayBank
mov 0b00100000 [0x15]
mov 0b01000000 [0x16]
mov 0b01111110 [0x17]
mov 0b00000000 [0x18]
jt MAIN_LOSS

PLAYER_L_LOSS:
sbsel displayBank
mov 0b00100010 [0x15]
mov 0b01000110 [0x16]
mov 0b01001010 [0x17]
mov 0b00110010 [0x18]
jt MAIN_LOSS

MAIN_LOSS:
mov 0b00000000 [0x10]
mov 0b01111110 [0x11]
mov 0b01010000 [0x12]
mov 0b00100000 [0x13]
mov 0b00000000 [0x14]

mov 0b00000000 [0x19]
mov 0b01111100 [0x1a]
mov 0b00000010 [0x1b]
mov 0b00111100 [0x1c]
mov 0b00000010 [0x1d]
mov 0b01111100 [0x1e]
mov 0b00000000 [0x1f]
sbsel mainBank

mov 0xff acc
WINNER_DISPLAY_LOOP:
sub 1 acc
jez SETUP
jt WINNER_DISPLAY_LOOP
