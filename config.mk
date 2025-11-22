PROJECTNAME:=bricked-up
BIN=${PROJECTNAME}.gb

# tools
RGBDS?=
ASSEMBLER:=${RGBDS}rgbasm
LINKER:=${RGBDS}rgblink
FIX:=${RGBDS}rgbfix
GFX:=${RGBDS}rgbgfx

# flags
FIXFLAGS:= -v -p 0xFF
