N418.c
#define PI 3.1416
#define F f
#define D /* Expands into no preprocessing tokens. */
#define LD L
#define glue(a, b) a ## b
#define xglue(a, b) glue(a, b)
/*
* The following:
*/
float f = xglue(PI, F);
double d = xglue(PI, D);
long double ld = xglue(PI, LD);
/*
* should expand into:
*/
float f = 3.1416f;
double d = 3.1416;
long double ld = 3.1416L;
