
#define L_PAREN (
#define F_M(a) a
char *p = F_M L_PAREN "abc" ); /* Expands to F_M("abc") not "abc" */
