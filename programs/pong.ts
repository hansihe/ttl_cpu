minusOne = 255

mainBank = 0b00000000
displayBank = 0b11111000
buttonBank = 0b11111001

ballX = 0x10
ballY = 0x11
ballDirX = 0x12
ballDirY = 0x13

paddleLY = 0x14
paddleRY = 0x15

r0 = 0x00
r1 = 0x01
r2 = 0x02
r3 = 0x03
r4 = 0x04
r5 = 0x05

SETUP:
# Initialize ball position
sbsel mainBank
li 4
sm ballX
sm ballY

# Initialize ball vector
li minusOne
sm ballDirX
sm ballDirY

# Initialize paddle positions
li 3
sm paddleLY
sm paddleRY

# Make paddle graphics LUT
li 0b11000000
sm 0xe0
li 0b11100000
sm 0xe1
li 0b01110000
sm 0xe2
li 0b00111000
sm 0xe3
li 0b00011100
sm 0xe4
li 0b00001110
sm 0xe5
li 0b00000111
sm 0xe6
li 0b00000011
sm 0xe7

# Make ball graphics LUT
li 0b10000000
sm 0xf0
li 0b01000000
sm 0xf1
li 0b00100000
sm 0xf2
li 0b00010000
sm 0xf3
li 0b00001000
sm 0xf4
li 0b00000100
sm 0xf5
li 0b00000010
sm 0xf6
li 0b00000001
sm 0xf7

sbsel displayBank
li 0
sm 0x10
sm 0x11
sm 0x12
sm 0x13
sm 0x14
sm 0x15
sm 0x16
sm 0x17
sm 0x18
sm 0x19
sm 0x1a
sm 0x1b
sm 0x1c
sm 0x1d
sm 0x1e
sm 0x1f
sbsel mainBank

MAIN_LOOP:

# === Paddle movement ===
# Get buttons
sbsel buttonBank
lm 0x10
sm r0
lm 0x11
sm r1
lm 0x12
sm r2
lm 0x13
sm r3
sbsel mainBank

# Apply paddle left
lm r0
jez SKIP_B1
lm paddleLY
addi 1
sm paddleLY
SKIP_B1:
lm r1
jez SKIP_B2
lm paddleLY
subi 1
sm paddleLY
SKIP_B2:

# Apply paddle right
lm r2
jez SKIP_B3
lm paddleRY
addi 1
sm paddleRY
SKIP_B3:
lm r3
jez SKIP_B4
lm paddleRY
subi 1
sm paddleRY
SKIP_B4:

# Check paddle left bounds
# Lower
lm paddleLY
jnlz SKIP_L_CLAMP_1
li 0
sm paddleLY
SKIP_L_CLAMP_1:
# Upper
lm paddleLY
subi 7
jlz SKIP_L_CLAMP_2
li 7
sm paddleLY
SKIP_L_CLAMP_2:

# Check paddle right bounds
# Lower
lm paddleRY
jnlz SKIP_R_CLAMP_1
li 0
sm paddleRY
SKIP_R_CLAMP_1:
# Upper
lm paddleRY
subi 7
jlz SKIP_R_CLAMP_2
li 7
sm paddleRY
SKIP_R_CLAMP_2:

# === Ball movement ===
# Move ball X
lm ballX
addm ballDirX
sm ballX

# Check ball lower bounds
lm ballX
jnez FINISH_SET_X_DIR_POSITIVE
li 1
sm ballDirX
FINISH_SET_X_DIR_POSITIVE:

# Check ball upper bounds
lm ballX
subi 7
jnez FINISH_SET_X_DIR_NEGATIVE
li minusOne
sm ballDirX
FINISH_SET_X_DIR_NEGATIVE:

# Move ball Y
lm ballY
addm ballDirY
sm ballY

# Check ball lower bounds
lm ballY
jnez FINISH_SET_Y_DIR_POSITIVE
li 1
sm ballDirY
FINISH_SET_Y_DIR_POSITIVE:

# Check ball upper bounds
lm ballY
subi 13
jnez FINISH_SET_Y_DIR_NEGATIVE
li minusOne
sm ballDirY
FINISH_SET_Y_DIR_NEGATIVE:

# === Render ball ===
# Fetch ball graphic
lm ballX
addi 0xf0 # Offset to start of ball graphics LUT
sm r0
lp r0 # Fetch value from LUT
sm r0 # Store graphic in correct line
lm ballY
addi 1 # Offset 1 from edge to make room for paddle
addi 0xa0 # Offset to start of graphics memory
sm r1
lm r0
sbsel displayBank
sp r1 # Store graphic to graphics memory

# Clear neighbouring lines
lm r1
subi 1
sm r1
li 0
sp r1
lm r1
addi 2
sm r1
li 0
sp r1
sbsel mainBank

# === Render paddles ===
# Fetch left paddle graphic
lm paddleLY
addi 0xe0
sm r0
lp r0
sm r1

# Fetch right paddle graphic
lm paddleRY
addi 0xe0
sm r0
lp r0
sm r2

sbsel displayBank
lm r1
sm 0x10
lm r2
sm 0x1f
sbsel mainBank

# === Check for loss ===
# Left
lm ballY
subi 0
jnez LEFT_NO_HIT
lm ballX
subm paddleLY
subi 1
jgz PLAYER_L_LOSS
addi 2
jlz PLAYER_L_LOSS
LEFT_NO_HIT:

# Right
lm ballY
subi 13
jnez RIGHT_NO_HIT
lm ballX
subm paddleRY
subi 1
jgz PLAYER_R_LOSS
addi 2
jlz PLAYER_R_LOSS
RIGHT_NO_HIT:

jt MAIN_LOOP

PLAYER_R_LOSS:
sbsel displayBank
li 0b00100000
sm 0x15
li 0b01000000
sm 0x16
li 0b01111110
sm 0x17
li 0b00000000
sm 0x18
jt MAIN_LOSS

PLAYER_L_LOSS:
sbsel displayBank
li 0b00100010
sm 0x15
li 0b01000110
sm 0x16
li 0b01001010
sm 0x17
li 0b00110010
sm 0x18
jt MAIN_LOSS

MAIN_LOSS:
li 0b00000000
sm 0x10
li 0b01111110
sm 0x11
li 0b01010000
sm 0x12
li 0b00100000
sm 0x13
li 0b00000000
sm 0x14

li 0b00000000
sm 0x19
li 0b01111100
sm 0x1a
li 0b00000010
sm 0x1b
li 0b00111100
sm 0x1c
li 0b00000010
sm 0x1d
li 0b01111100
sm 0x1e
li 0b00000000
sm 0x1f
sbsel mainBank

li 0xff
WINNER_DISPLAY_LOOP:
subi 1
jez SETUP
jt WINNER_DISPLAY_LOOP





