#define mkstr(x) # x
char *p = mkstr(a \ b); /* "a \ b" violates the syntax of string literals */
