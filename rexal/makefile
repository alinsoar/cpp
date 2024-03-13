
RA=racket

include ../asdl/makefile-asdl

do:
	${RA} test.rkt

desugar:
	${RA} desugar.rkt $D $O.desugared

regex: ASDL_GENERATOR=../asdl/generic-asdl-gen.rkt
regex: regex.code

op: ASDL_GENERATOR=op.asdlgen.rkt
op: op.code

clean: clean-asdl
	@echo ok;


