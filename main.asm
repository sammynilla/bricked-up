INCLUDE "hardware.inc"

DEF BRICK_LEFT    EQU $05
DEF BRICK_RIGHT   EQU $06
DEF BLANK_TILE    EQU $08
DEF DIGIT_OFFSET  EQU $1A
DEF SCORE_TENS    EQU $9870
DEF SCORE_ONES    EQU $9871

SECTION "Header", ROM0[$100]
  JP EntryPoint
  DS $150 - @, 0 ; make room for the header
EntryPoint:
  ; do not turn the LCD off outside of VBlank
WaitVBlank:
  LD A, [rLY]
  CP 144
  JP c, WaitVBlank
  ; turn the LCD off
  LD A, 0
  LD [rLCDC], A
; copy the tiles
  LD DE, Tiles
  LD HL, $9000
  LD BC, TilesEnd - Tiles
  CALL Memcopy
; copy the tilemap
  LD DE, Tilemap
  LD HL, $9800
  LD BC, TilemapEnd - Tilemap
  CALL Memcopy
; copy the paddle tile
  LD DE, Paddle
  LD HL, $8000
  LD BC, PaddleEnd - Paddle
  call Memcopy
; copy the ball
  LD DE, Ball
  LD HL, $8010
  LD BC, BallEnd - Ball
  CALL Memcopy
; clear oam
  XOR A, A
  LD B, 160
  LD HL, _OAMRAM
ClearOam:
  LD [HLi], A
  DEC B
  JP nz, ClearOam
; init game object (paddle)
  LD HL, _OAMRAM
  LD A, 128 + 16
  LD [HLi], A
  LD a, 16 + 8
  LD [HLi], A
  LD a, 0
  LD [HLi], A
  LD [HLi], A
; init game object (ball)
  LD A, 100 + 16
  LD [HLi], A
  LD A, 32 + 8
  LD [HLi], A
  LD A, 1
  LD [HLi], A
  LD A, 0
  LD [HLi], A
  ; set ball momentum (up and right)
  LD A, 1
  LD [wBallMomentumX], A
  LD A, -1
  LD [wBallMomentumY], A
; enable the LCD
  LD A, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
  LD [rLCDC], A
  ; during the first (blank) frame, initialize display registers
  LD A, %11100100
  LD [rBGP], A
  LD A, %11100100
  LD [rOBP0], A
; initialize global vars
  LD A, 0
  LD [wFrameCounter], A
  LD [wCurKeys], A
  LD [wNewKeys], A
  LD [wScore], A
Main:
  ; wait until no longer VBlank
  LD A, [rLY]
  CP 144
  JP nc, Main
WaitVBlank2:
  LD A, [rLY]
  CP 144
  JP c, WaitVBlank2
; update the ball's position using its momentum
  LD A, [wBallMomentumX]
  LD B, A
  LD A, [_OAMRAM + 5]
  ADD A, B
  LD [_OAMRAM + 5], A
  LD A, [wBallMomentumY]
  LD B, A
  LD A, [_OAMRAM + 4]
  ADD A, B
  LD [_OAMRAM + 4], A
BounceOnTop:
  LD A, [_OAMRAM + 4]
  SUB A, 16 + 1
  LD C, A
  LD A, [_OAMRAM + 5]
  SUB A, 8
  LD B, A
  CALL GetTileByPixel ; returns tile address in hl
  LD A, [HL]
  CALL IsWallTile
  JP nz, BounceOnRight
  CALL CheckAndHandleBrick
  LD A, 1
  LD [wBallMomentumY], A
BounceOnRight:
  LD A, [_OAMRAM + 4]
  SUB A, 16
  LD C, A
  LD A, [_OAMRAM + 5]
  SUB A, 8 - 5
  LD B, A
  CALL GetTileByPixel ; returns tile address in hl
  LD A, [HL]
  CALL IsWallTile
  JP nz, BounceOnLeft
  CALL CheckAndHandleBrick
  LD A, -1
  LD [wBallMomentumX], A
BounceOnLeft:
  LD A, [_OAMRAM + 4]
  SUB A, 16
  LD C, A
  LD A, [_OAMRAM + 5]
  SUB A, 8 + 1
  LD B, A
  CALL GetTileByPixel ; returns tile address in hl
  LD A, [HL]
  CALL IsWallTile
  JP nz, BounceOnBottom
  CALL CheckAndHandleBrick
  LD A, 1
  LD [wBallMomentumX], A
BounceOnBottom:
  LD A, [_OAMRAM + 4]
  SUB A, 16 - 1
  LD C, A
  LD A, [_OAMRAM + 5]
  SUB A, 8
  LD B, A
  CALL GetTileByPixel ; returns tile address in hl
  LD A, [HL]
  CALL IsWallTile
  JP nz, BounceDone
  CALL CheckAndHandleBrick
  LD A, -1
  LD [wBallMomentumY], A
BounceDone:
; check if ball is low enough to bounce off the paddle
  LD A, [_OAMRAM]
  LD B, A
  LD A, [_OAMRAM + 4]
  ADD A, 4
  CP A, B
  JP nz, PaddleBounceDone ; check to make sure that ball and paddle have the same y position
  LD A, [_OAMRAM + 5]
  LD B, A
  LD A, [_OAMRAM + 1]
  SUB A, 4 + 2      ; subtract half paddle width from paddle x position
  CP A, B           ; sets carry flag if a < b
  JP nc, PaddleBounceDone
  ADD A, 4 + 8 + 2  ; add half paddle width to x value to return to original position, add paddle width to x value
  CP A, B
  JP c, PaddleBounceDone
  ; bounce happened, change momentum
  LD A, -1
  LD [wBallMomentumY], A
PaddleBounceDone:
; check the current keys every frame and move left or right
  CALL UpdateKeys
; check if left button is pressed
CheckLeft:
  LD A, [wCurKeys]
  AND A, PADF_LEFT
  JP z, CheckRight
Left:
  ; move the paddle one pixel to the left
  LD A, [_OAMRAM + 1]
  DEC A
  CP A, 15 ; edge of playfield check
  JP z, Main
  LD [_OAMRAM + 1], A
  JP Main
; check if right button is pressed
CheckRight:
  LD A, [wCurKeys]
  AND A, PADF_RIGHT
  JP z, Main
Right:
  ; move the paddle one pixel to the right
  LD A, [_OAMRAM + 1]
  INC A
  CP A, 105 ; edge of playfield check
  JP z, Main
  LD [_OAMRAM + 1], A
  JP Main
; check if a brick was collided with and break it if possible
; @params hl: address of tile
CheckAndHandleBrick:
  LD A, [HL]
  CP A, BRICK_LEFT
  JR nz, CheckAndHandleBrickRight
  ; break the brick from the left side
  LD [HL], BLANK_TILE
  INC HL
  LD [HL], BLANK_TILE
  CALL IncreaseScorePackedBCD
CheckAndHandleBrickRight:
  CP A, BRICK_RIGHT
  RET nz
  ; break the brick from the right side
  LD [HL], BLANK_TILE
  DEC HL
  LD [HL], BLANK_TILE
  CALL IncreaseScorePackedBCD
  RET

UpdateKeys: ; mysterious input polling black box from https://gbdev.io/gb-asm-tutorial/part2/input.html
  ; Poll half the controller
  LD A, P1F_GET_BTN
  CALL .onenibble
  LD B, A   ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  LD A, P1F_GET_DPAD
  CALL .onenibble
  SWAP A    ; A7-4 = unpressed directions; A3-0 = 1
  XOR A, B  ; A = pressed buttons + directions
  LD B, A   ; B = pressed buttons + directions

  ; And release the controller
  LD A, P1F_GET_NONE
  LDH [rP1], A

  ; Combine with previous wCurKeys to make wNewKeys
  LD A, [wCurKeys]
  XOR A, B ; A = keys that changed state
  AND A, B ; A = keys that changed to pressed
  LD [wNewKeys], A
  LD A, B
  LD [wCurKeys], A
  RET
.onenibble
  LDH [rP1], A    ; switch the key matrix
  CALL .knownret  ; burn 10 cycles calling a known ret
  LDH A, [rP1]    ; ignore value while waiting for the key matrix to settle
  LDH A, [rP1]
  LDH A, [rP1]    ; this read counts
  OR A, $F0       ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  RET
; copy bytes from one area to another
; @param de: source
; @param hl: destination
; @param bc: length
Memcopy:
  LD A, [DE]
  LD [HLi], A
  INC DE
  DEC BC
  LD A, B
  OR A, C
  JP nz, Memcopy
  RET
; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:
  ; First, we need to divide by 8 to convert a pixel position to a tile position.
  ; After this we want to multiply the Y position by 32.
  ; These operations effectively cancel out so we only need to mask the Y value.
  LD A, C
  AND A, %11111000
  LD L, A
  LD H, 0
  ; Now we have the position * 8 in hl
  ADD HL, HL ; position * 16
  ADD HL, HL ; position * 32
  ; Convert the X position to an offset.
  LD A, B
  SRL A ; a / 2
  SRL A ; a / 4
  SRL A ; a / 8
  ; Add the two offsets together.
  ADD A, L
  LD L, A
  ADC A, H
  SUB A, L
  LD H, A
  ; Add the offset to the tilemap's base address, and we are done!
  LD BC, $9800
  ADD HL, BC
  RET
; increases score by 1 and store it as a 1 byte packed BCD (binary coded decimal) value
; changes A and HL
IncreaseScorePackedBCD:
  XOR A         ; clear out the carry flag and value in the accumulator
  INC A         ; a = 1
  LD HL, wScore ; load score into HL
  ADC [HL]      ; add 1
  DAA           ; convert to BCD
  LD [HL], A    ; store score in HL
  CALL UpdateScoreBoard
  RET
; read the packed BCD value from wScore and update the score display
UpdateScoreBoard:
  LD A, [wScore]
  AND %11110000 ; mask the lower nibble
  SWAP A        ; move the upper nibble to the lower nibble
  ADD A, DIGIT_OFFSET
  LD [SCORE_TENS], A
  LD A, [wScore]
  AND %00001111 ; mask the upper nibble
  ADD A, DIGIT_OFFSET
  LD [SCORE_ONES], A
; @param a: tile ID
; @return z: set if a is a wall.
IsWallTile:
  CP A, $00
  RET z
  CP A, $01
  RET z
  CP A, $02
  RET z
  CP A, $04
  RET z
  CP A, $05
  RET z
  CP A, $06
  RET z
  CP A, $07
  RET

Tiles:
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33322222
  DW `33322222
  DW `33322222
  DW `33322211
  DW `33322211
  DW `33333333
  DW `33333333
  DW `33333333
  DW `22222222
  DW `22222222
  DW `22222222
  DW `11111111
  DW `11111111
  DW `33333333
  DW `33333333
  DW `33333333
  DW `22222333
  DW `22222333
  DW `22222333
  DW `11222333
  DW `11222333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33333333
  DW `33322211
  DW `33322211
  DW `33322211
  DW `33322211
  DW `33322211
  DW `33322211
  DW `33322211
  DW `33322211
  DW `22222222
  DW `20000000
  DW `20111111
  DW `20111111
  DW `20111111
  DW `20111111
  DW `22222222
  DW `33333333
  DW `22222223
  DW `00000023
  DW `11111123
  DW `11111123
  DW `11111123
  DW `11111123
  DW `22222223
  DW `33333333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `11222333
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `11001100
  DW `11111111
  DW `11111111
  DW `21212121
  DW `22222222
  DW `22322232
  DW `23232323
  DW `33333333
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222211
  DW `22222211
  DW `22222211
  DW `22222222
  DW `22222222
  DW `22222222
  DW `11111111
  DW `11111111
  DW `11221111
  DW `11221111
  DW `11000011
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `11222222
  DW `11222222
  DW `11222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222211
  DW `22222200
  DW `22222200
  DW `22000000
  DW `22000000
  DW `22222222
  DW `22222222
  DW `22222222
  DW `11000011
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11000022
  DW `11222222
  DW `11222222
  DW `11222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222200
  DW `22222200
  DW `22222211
  DW `22222211
  DW `22221111
  DW `22221111
  DW `22221111
  DW `11000022
  DW `00112222
  DW `00112222
  DW `11112200
  DW `11112200
  DW `11220000
  DW `11220000
  DW `11220000
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22000000
  DW `22000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `22222222
  DW `11110022
  DW `11110022
  DW `11110022
  DW `22221111
  DW `22221111
  DW `22221111
  DW `22221111
  DW `22221111
  DW `22222211
  DW `22222211
  DW `22222222
  DW `11220000
  DW `11110000
  DW `11110000
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `22222222
  DW `00000000
  DW `00111111
  DW `00111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `11111111
  DW `22222222
  DW `11110022
  DW `11000022
  DW `11000022
  DW `00002222
  DW `00002222
  DW `00222222
  DW `00222222
  DW `22222222
; digits
  ; 0
  DW `33333333
  DW `33000033
  DW `30033003
  DW `30033003
  DW `30033003
  DW `30033003
  DW `33000033
  DW `33333333
  ; 1
  DW `33333333
  DW `33300333
  DW `33000333
  DW `33300333
  DW `33300333
  DW `33300333
  DW `33000033
  DW `33333333
  ; 2
  DW `33333333
  DW `33000033
  DW `30330003
  DW `33330003
  DW `33000333
  DW `30003333
  DW `30000003
  DW `33333333
  ; 3
  DW `33333333
  DW `30000033
  DW `33330003
  DW `33000033
  DW `33330003
  DW `33330003
  DW `30000033
  DW `33333333
  ; 4
  DW `33333333
  DW `33000033
  DW `30030033
  DW `30330033
  DW `30330033
  DW `30000003
  DW `33330033
  DW `33333333
  ; 5
  DW `33333333
  DW `30000033
  DW `30033333
  DW `30000033
  DW `33330003
  DW `30330003
  DW `33000033
  DW `33333333
  ; 6
  DW `33333333
  DW `33000033
  DW `30033333
  DW `30000033
  DW `30033003
  DW `30033003
  DW `33000033
  DW `33333333
  ; 7
  DW `33333333
  DW `30000003
  DW `33333003
  DW `33330033
  DW `33300333
  DW `33000333
  DW `33000333
  DW `33333333
  ; 8
  DW `33333333
  DW `33000033
  DW `30333003
  DW `33000033
  DW `30333003
  DW `30333003
  DW `33000033
  DW `33333333
  ; 9
  DW `33333333
  DW `33000033
  DW `30330003
  DW `30330003
  DW `33000003
  DW `33330003
  DW `33000033
  DW `33333333
TilesEnd:
Tilemap:
  DB $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $1A, $1A, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0A, $0B, $0C, $0D, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0E, $0F, $10, $11, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $12, $13, $14, $15, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $16, $17, $18, $19, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  DB $04, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:
Paddle:
  DW `33333333
  DW `32222223
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
  DW `00000000
PaddleEnd:
Ball:
  DW `03330000
  DW `32223000
  DW `32223000
  DW `32223000
  DW `03330000
  DW `00000000
  DW `00000000
  DW `00000000
BallEnd:

SECTION "Counter", WRAM0
wFrameCounter: DB

SECTION "Input Variables", WRAM0
wCurKeys: DB
wNewKeys: DB

SECTION "Ball Data", WRAM0
wBallMomentumX: DB
wBallMomentumY: DB

SECTION "Score", WRAM0
wScore: DB
