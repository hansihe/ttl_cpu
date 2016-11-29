minusOne = 255

mainBank = 0b00000000
displayBank = 0b11111000

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

MAIN_LOOP:

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

jt MAIN_LOOP
