include config.mk
.PHONY: clean

main: main.o
	${LINKER} -o ${BIN} $<
	${FIX} ${FIXFLAGS} ${BIN}

%.o:: %.gbasm
	${ASSEMBLER} -o $@ $<

clean:
	@rm *.o
	@rm ${BIN}
