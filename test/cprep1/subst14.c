#define GLUE(a, b, c) a ## b ## c
extern void f(int p, GLUE(. , . , .));
#include GLUE(< , header, >)


