include config.mk
.PHONY: clean

main: main.o
	${LINKER} -o ${BIN} $<
	${FIX} -v -p 0xFF ${BIN}

%.o:: %.gbasm
	${ASSEMBLER} -o $@ $<

clean:
	@rm *.o
	@rm ${BIN}
