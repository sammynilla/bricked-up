INCLUDE "hardware.inc"

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
  ld [hl], a
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
Main:
  ; wait until no longer VBlank
  ld a, [rLY]
  cp 144
  jp nc, Main
WaitVBlank2:
  ld a, [rLY]
  cp 144
  jp c, WaitVBlank2
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
TilesEnd:
Tilemap:
  db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
  db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
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
  dw `03333330
  dw `00000000
  dw `00333300
  dw `00000000
  dw `00033000
  dw `00000000
  dw `00000000
PaddleEnd:

SECTION "Counter", WRAM0
wFrameCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db
