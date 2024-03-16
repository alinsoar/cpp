

# Description

A C preprocessor written in scheme.

# Examples


```
% racket cc.rkt ./test/cprep1/incl.c
 Compile ./test/cprep1/incl.c
 .CPP ./test/cprep1/incl.c
 (CTOK.ASTERISK 4 0 ./test/cprep1/incl.c)
 (CTOK.ASTERISK 4 1 ./test/cprep1/incl.c)
 (CTOK.ID main 5 0 ./test/cprep1/incl.c)
 (CTOK.OPEN_BRACE 6 0 ./test/cprep1/incl.c)
 (CTOK.INTEGER 10 int 5 10 /work/C-PREPROCESSOR/ccomp/test/cprep1/incl.h)
 (CTOK.ADDITION 7 3 ./test/cprep1/incl.c)
 (CTOK.INTEGER 1 int 7 4 ./test/cprep1/incl.c)
 (CTOK.SEMICOLON 7 5 ./test/cprep1/incl.c)
 (CTOK.CLOSE_BRACE 8 0 ./test/cprep1/incl.c)
 FINE: 0
```



```
% racket cc.rkt ./test/cprep1/char.c
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

% racket cc.rkt ./test/ioccc-pp-test/2000/primenum.c
 Compile ./test/ioccc-pp-test/2000/primenum.c
 .CPP ./test/ioccc-pp-test/2000/primenum.c
 (CTOK.VOID 1 21 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID main 1 26 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 1 30 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INT 1 31 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID argc 1 35 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.COMMA 1 39 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CHAR 1 41 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASTERISK 1 46 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID argv 1 47 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_BRACKET 1 51 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_BRACKET 1 52 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 1 53 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_BRACE 16 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INT 4 16 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.COMMA 4 21 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 4 22 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.COMMA 4 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID n 4 24 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.COMMA 4 25 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID Flag 4 26 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 4 30 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 1 int 4 31 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 4 32 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CHAR 25 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID c 25 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 25 24 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID n 8 31 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 8 32 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID atoi 8 33 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 8 37 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID argv 8 38 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_BRACKET 8 42 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 1 int 8 43 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_BRACKET 8 44 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 8 45 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 8 46 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 7 16 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.FOR 7 17 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 33 9 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 22 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 2 int 23 22 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 33 44 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 19 22 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID c 19 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 19 24 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID getchar 19 25 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 19 32 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 19 33 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 19 34 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.DIFFERENT 14 26 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SUBTRACTION 3 28 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 1 int 3 29 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 34 62 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INCREMENTER 11 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 35 33 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_BRACE 16 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 37 2 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 22 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 1 int 24 13 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 7 16 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.FOR 7 17 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 38 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 17 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 22 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 2 int 23 22 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 38 46 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 17 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASTERISK 13 17 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 13 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID c 13 19 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.AND 13 20 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 64 int 13 21 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 13 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASTERISK 13 24 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 39 49 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INCREMENTER 11 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 17 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 40 35 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.IF 10 11 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 41 6 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.REMAINDER 15 15 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 17 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.EQUAL 12 11 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.REMAINDER 26 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID j 26 15 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 41 50 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 42 4 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASSIGN 22 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 0 int 6 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 6 15 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.IF 10 11 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 44 5 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID Flag 44 6 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.EQUAL 12 11 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 1 int 24 13 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 44 18 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID putchar 21 14 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 20 20 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID c 20 21 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.EXCLUSIVE_OR 20 22 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.OPEN_PAREN 20 23 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.LOGNOT 20 24 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID F1ag 9 10 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ASTERISK 20 26 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.ID n 20 27 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.REMAINDER 20 28 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.INTEGER 64 int 20 29 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 20 31 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_PAREN 20 32 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 20 33 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_BRACE 2 19 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.RETURN 5 21 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.SEMICOLON 5 27 ./test/ioccc-pp-test/2000/primenum.c)
 (CTOK.CLOSE_BRACE 2 19 ./test/ioccc-pp-test/2000/primenum.c)
 FINE: 0
```
 
