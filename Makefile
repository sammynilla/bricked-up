include config.mk
.PHONY: clean

main: main.o
	${LINKER} -o ${BIN} $<
	${FIX} ${FIXFLAGS} ${BIN}

%.o:: %.asm
	${ASSEMBLER} -o $@ $<

clean:
	@rm *.o
	@rm ${BIN}
