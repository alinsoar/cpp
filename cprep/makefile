
include ../asdl/makefile-asdl

.PRECIOUS: %.def

pp: ASDL_GENERATOR=../asdl/generic-asdl-gen.rkt
pp: pp.code
	@echo ok;

pptriedata: ASDL_GENERATOR=pptriedata.asdlgen.rkt
pptriedata: pptriedata.code

c-lexical-grammar:
	racket ../lexer/compile.rkt c-lexical.bnf $@

clean: clean-asdl
#	rm c-lexical-grammar
	@echo ok;

