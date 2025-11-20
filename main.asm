INCLUDE "hardware.inc"

DEF BRICK_LEFT    EQU $05
DEF BRICK_RIGHT   EQU $06
DEF BLANK_TILE    EQU $08
DEF DIGIT_OFFSET  EQU $1A
DEF SCORE_TENS    EQU $9870
DEF SCORE_ONES    EQU $9871

SECTION "Header", ROM0[$100]
  jp EntryPoint
  ds $150 - @, 0 ; make room for the header
EntryPoint:
  ; do not turn the LCD off outside of VBlank
WaitVBlank:
  ld a, [rLY]
  cp 144
  jp c, WaitVBlank
  ; turn the LCD off
  ld a, 0
  ld [rLCDC], a
; copy the tiles
  ld de, Tiles
  ld hl, $9000
  ld bc, TilesEnd - Tiles
  call Memcopy
; copy the tilemap
  ld de, Tilemap
  ld hl, $9800
  ld bc, TilemapEnd - Tilemap
  call Memcopy
; copy the paddle tile
  ld de, Paddle
  ld hl, $8000
  ld bc, PaddleEnd - Paddle
  call Memcopy
; copy the ball
  ld de, Ball
  ld hl, $8010
  ld bc, BallEnd - Ball
  call Memcopy
; clear oam
  xor a, a
  ld b, 160
  ld hl, _OAMRAM
ClearOam:
  ld [hli], a
  dec b
  jp nz, ClearOam
; init game object (paddle)
  ld hl, _OAMRAM
  ld a, 128 + 16
  ld [hli], a
  ld a, 16 + 8
  ld [hli], a
  ld a, 0
  ld [hli], a
  ld [hli], a
; init game object (ball)
  ld a, 100 + 16
  ld [hli], a
  ld a, 32 + 8
  ld [hli], a
  ld a, 1
  ld [hli], a
  ld a, 0
  ld [hli], a
  ; set ball momentum (up and right)
  ld a, 1
  ld [wBallMomentumX], a
  ld a, -1
  ld [wBallMomentumY], a
; enable the LCD
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
  ld [rLCDC], a
  ; during the first (blank) frame, initialize display registers
  ld a, %11100100
  ld [rBGP], a
  ld a, %11100100
  ld [rOBP0], a
; initialize global vars
  ld a, 0
  ld [wFrameCounter], a
  ld [wCurKeys], a
  ld [wNewKeys], a
  ld [wScore], a
Main:
  ; wait until no longer VBlank
  ld a, [rLY]
  cp 144
  jp nc, Main
WaitVBlank2:
  ld a, [rLY]
  cp 144
  jp c, WaitVBlank2
; update the ball's position using its momentum
  ld a, [wBallMomentumX]
  ld b, a
  ld a, [_OAMRAM + 5]
  add a, b
  ld [_OAMRAM + 5], a
  ld a, [wBallMomentumY]
  ld b, a
  ld a, [_OAMRAM + 4]
  add a, b
  ld [_OAMRAM + 4], a
BounceOnTop:
  ld a, [_OAMRAM + 4]
  sub a, 16 + 1
  ld c, a
  ld a, [_OAMRAM + 5]
  sub a, 8
  ld b, a
  call GetTileByPixel ; returns tile address in hl
  ld a, [hl]
  call IsWallTile
  jp nz, BounceOnRight
  call CheckAndHandleBrick
  ld a, 1
  ld [wBallMomentumY], a
BounceOnRight:
  ld a, [_OAMRAM + 4]
  sub a, 16
  ld c, a
  ld a, [_OAMRAM + 5]
  sub a, 8 - 5
  ld b, a
  call GetTileByPixel ; returns tile address in hl
  ld a, [hl]
  call IsWallTile
  jp nz, BounceOnLeft
  call CheckAndHandleBrick
  ld a, -1
  ld [wBallMomentumX], a
BounceOnLeft:
  ld a, [_OAMRAM + 4]
  sub a, 16
  ld c, a
  ld a, [_OAMRAM + 5]
  sub a, 8 + 1
  ld b, a
  call GetTileByPixel ; returns tile address in hl
  ld a, [hl]
  call IsWallTile
  jp nz, BounceOnBottom
  call CheckAndHandleBrick
  ld a, 1
  ld [wBallMomentumX], a
BounceOnBottom:
  ld a, [_OAMRAM + 4]
  sub a, 16 - 1
  ld c, a
  ld a, [_OAMRAM + 5]
  sub a, 8
  ld b, a
  call GetTileByPixel ; returns tile address in hl
  ld a, [hl]
  call IsWallTile
  jp nz, BounceDone
  call CheckAndHandleBrick
  ld a, -1
  ld [wBallMomentumY], a
BounceDone:
; check if ball is low enough to bounce off the paddle
  ld a, [_OAMRAM]
  ld b, a
  ld a, [_OAMRAM + 4]
  add a, 4
  cp a, b
  jp nz, PaddleBounceDone ; check to make sure that ball and paddle have the same y position
  ld a, [_OAMRAM + 5]
  ld b, a
  ld a, [_OAMRAM + 1]
  sub a, 4 + 2      ; subtract half paddle width from paddle x position
  cp a, b           ; sets carry flag if a < b
  jp nc, PaddleBounceDone
  add a, 4 + 8 + 2  ; add half paddle width to x value to return to original position, add paddle width to x value
  cp a, b
  jp c, PaddleBounceDone
  ; bounce happened, change momentum
  ld a, -1
  ld [wBallMomentumY], a
PaddleBounceDone:
; check the current keys every frame and move left or right
  call UpdateKeys
; check if left button is pressed
CheckLeft:
  ld a, [wCurKeys]
  and a, PADF_LEFT
  jp z, CheckRight
Left:
  ; move the paddle one pixel to the left
  ld a, [_OAMRAM + 1]
  dec a
  cp a, 15 ; edge of playfield check
  jp z, Main
  ld [_OAMRAM + 1], a
  jp Main
; check if right button is pressed
CheckRight:
  ld a, [wCurKeys]
  and a, PADF_RIGHT
  jp z, Main
Right:
  ; move the paddle one pixel to the right
  ld a, [_OAMRAM + 1]
  inc a
  cp a, 105 ; edge of playfield check
  jp z, Main
  ld [_OAMRAM + 1], a
  jp Main
; check if a brick was collided with and break it if possible
; @params hl: address of tile
CheckAndHandleBrick:
  ld a, [hl]
  cp a, BRICK_LEFT
  jr nz, CheckAndHandleBrickRight
  ; break the brick from the left side
  ld [hl], BLANK_TILE
  inc hl
  ld [hl], BLANK_TILE
  call IncreaseScorePackedBCD
CheckAndHandleBrickRight:
  cp a, BRICK_RIGHT
  ret nz
  ; break the brick from the right side
  ld [hl], BLANK_TILE
  dec hl
  ld [hl], BLANK_TILE
  call IncreaseScorePackedBCD
  ret

UpdateKeys: ; mysterious input polling black box from https://gbdev.io/gb-asm-tutorial/part2/input.html
  ; Poll half the controller
  ld a, P1F_GET_BTN
  call .onenibble
  ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a, P1F_GET_DPAD
  call .onenibble
  swap a ; A7-4 = unpressed directions; A3-0 = 1
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; And release the controller
  ld a, P1F_GET_NONE
  ldh [rP1], a

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret
.onenibble
  ldh [rP1], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
  ldh a, [rP1]
  ldh a, [rP1] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret
; copy bytes from one area to another
; @param de: source
; @param hl: destination
; @param bc: length
Memcopy:
  ld a, [de]
  ld [hli], a
  inc de
  dec bc
  ld a, b
  or a, c
  jp nz, Memcopy
  ret
; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:
  ; First, we need to divide by 8 to convert a pixel position to a tile position.
  ; After this we want to multiply the Y position by 32.
  ; These operations effectively cancel out so we only need to mask the Y value.
  ld a, c
  and a, %11111000
  ld l, a
  ld h, 0
  ; Now we have the position * 8 in hl
  add hl, hl ; position * 16
  add hl, hl ; position * 32
  ; Convert the X position to an offset.
  ld a, b
  srl a ; a / 2
  srl a ; a / 4
  srl a ; a / 8
  ; Add the two offsets together.
  add a, l
  ld l, a
  adc a, h
  sub a, l
  ld h, a
  ; Add the offset to the tilemap's base address, and we are done!
  ld bc, $9800
  add hl, bc
  ret
; increases score by 1 and store it as a 1 byte packed BCD (binary coded decimal) value
; changes A and HL
IncreaseScorePackedBCD:
  xor a         ; clear out the carry flag and value in the accumulator
  inc a         ; a = 1
  ld hl, wScore ; load score into HL
  adc [hl]      ; add 1
  daa           ; convert to 
  ld [hl], a    ; store score in HL
  call UpdateScoreBoard
  ret
; read the packed BCD value from wScore and update the score display
UpdateScoreBoard:
  ld a, [wScore]
  and %11110000 ; mask the lower nibble
  swap a        ; move the upper nibble to the lower nibble
  add a, DIGIT_OFFSET
  ld [SCORE_TENS], a
  ld a, [wScore]
  and %00001111 ; mask the upper nibble
  add a, DIGIT_OFFSET
  ld [SCORE_ONES], a
; @param a: tile ID
; @return z: set if a is a wall.
IsWallTile:
  cp a, $00
  ret z
  cp a, $01
  ret z
  cp a, $02
  ret z
  cp a, $04
  ret z
  cp a, $05
  ret z
  cp a, $06
  ret z
  cp a, $07
  ret

Tiles:
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33322222
  dw `33322222
  dw `33322222
  dw `33322211
  dw `33322211
  dw `33333333
  dw `33333333
  dw `33333333
  dw `22222222
  dw `22222222
  dw `22222222
  dw `11111111
  dw `11111111
  dw `33333333
  dw `33333333
  dw `33333333
  dw `22222333
  dw `22222333
  dw `22222333
  dw `11222333
  dw `11222333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33322211
  dw `33322211
  dw `33322211
  dw `33322211
  dw `33322211
  dw `33322211
  dw `33322211
  dw `33322211
  dw `22222222
  dw `20000000
  dw `20111111
  dw `20111111
  dw `20111111
  dw `20111111
  dw `22222222
  dw `33333333
  dw `22222223
  dw `00000023
  dw `11111123
  dw `11111123
  dw `11111123
  dw `11111123
  dw `22222223
  dw `33333333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `11222333
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `11001100
  dw `11111111
  dw `11111111
  dw `21212121
  dw `22222222
  dw `22322232
  dw `23232323
  dw `33333333
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222211
  dw `22222211
  dw `22222211
  dw `22222222
  dw `22222222
  dw `22222222
  dw `11111111
  dw `11111111
  dw `11221111
  dw `11221111
  dw `11000011
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `11222222
  dw `11222222
  dw `11222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222211
  dw `22222200
  dw `22222200
  dw `22000000
  dw `22000000
  dw `22222222
  dw `22222222
  dw `22222222
  dw `11000011
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11000022
  dw `11222222
  dw `11222222
  dw `11222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222200
  dw `22222200
  dw `22222211
  dw `22222211
  dw `22221111
  dw `22221111
  dw `22221111
  dw `11000022
  dw `00112222
  dw `00112222
  dw `11112200
  dw `11112200
  dw `11220000
  dw `11220000
  dw `11220000
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22000000
  dw `22000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `22222222
  dw `11110022
  dw `11110022
  dw `11110022
  dw `22221111
  dw `22221111
  dw `22221111
  dw `22221111
  dw `22221111
  dw `22222211
  dw `22222211
  dw `22222222
  dw `11220000
  dw `11110000
  dw `11110000
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `22222222
  dw `00000000
  dw `00111111
  dw `00111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `11111111
  dw `22222222
  dw `11110022
  dw `11000022
  dw `11000022
  dw `00002222
  dw `00002222
  dw `00222222
  dw `00222222
  dw `22222222
; digits
  ; 0
  dw `33333333
  dw `33000033
  dw `30033003
  dw `30033003
  dw `30033003
  dw `30033003
  dw `33000033
  dw `33333333
  ; 1
  dw `33333333
  dw `33300333
  dw `33000333
  dw `33300333
  dw `33300333
  dw `33300333
  dw `33000033
  dw `33333333
  ; 2
  dw `33333333
  dw `33000033
  dw `30330003
  dw `33330003
  dw `33000333
  dw `30003333
  dw `30000003
  dw `33333333
  ; 3
  dw `33333333
  dw `30000033
  dw `33330003
  dw `33000033
  dw `33330003
  dw `33330003
  dw `30000033
  dw `33333333
  ; 4
  dw `33333333
  dw `33000033
  dw `30030033
  dw `30330033
  dw `30330033
  dw `30000003
  dw `33330033
  dw `33333333
  ; 5
  dw `33333333
  dw `30000033
  dw `30033333
  dw `30000033
  dw `33330003
  dw `30330003
  dw `33000033
  dw `33333333
  ; 6
  dw `33333333
  dw `33000033
  dw `30033333
  dw `30000033
  dw `30033003
  dw `30033003
  dw `33000033
  dw `33333333
  ; 7
  dw `33333333
  dw `30000003
  dw `33333003
  dw `33330033
  dw `33300333
  dw `33000333
  dw `33000333
  dw `33333333
  ; 8
  dw `33333333
  dw `33000033
  dw `30333003
  dw `33000033
  dw `30333003
  dw `30333003
  dw `33000033
  dw `33333333
  ; 9
  dw `33333333
  dw `33000033
  dw `30330003
  dw `30330003
  dw `33000003
  dw `33330003
  dw `33000033
  dw `33333333
TilesEnd:
Tilemap:
  db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $1A, $1A, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0A, $0B, $0C, $0D, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0E, $0F, $10, $11, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $12, $13, $14, $15, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $16, $17, $18, $19, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:
Paddle:
  dw `33333333
  dw `32222223
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
PaddleEnd:
Ball:
  dw `03330000
  dw `32223000
  dw `32223000
  dw `32223000
  dw `03330000
  dw `00000000
  dw `00000000
  dw `00000000
BallEnd:

SECTION "Counter", WRAM0
wFrameCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db

SECTION "Ball Data", WRAM0
wBallMomentumX: db
wBallMomentumY: db

SECTION "Score", WRAM0
wScore: db
