# cpp
A C preprocessor written in scheme.

Example:


racket cc.rkt ./test/cprep1/char.c
 Compile ./test/cprep1/char.c
 .CPP ./test/cprep1/char.c
 (CTOK.ID A 5 0 ./test/cprep1/char.c)
 (CTOK.ID B 7 0 ./test/cprep1/char.c)
 (CTOK.ID C 9 0 ./test/cprep1/char.c)
 (CTOK.ID D 11 0 ./test/cprep1/char.c)
 (CTOK.ID E 15 0 ./test/cprep1/char.c)
 (CTOK.ID F 17 0 ./test/cprep1/char.c)
 (CTOK.ID H 22 0 ./test/cprep1/char.c)
 (CTOK.ID I 24 0 ./test/cprep1/char.c)
 FINE: 0

 
