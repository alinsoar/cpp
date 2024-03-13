#define
LPAREN (
#define
RPAREN )
#define
F(x, y) x + y
#define
ELLIP_FUNC(...) __VA_ARGS__
ELLIP_FUNC(F, LPAREN, ’a’, ’b’, RPAREN); /* 1st invocation */
ELLIP_FUNC(F LPAREN ’a’, ’b’ RPAREN); /* 2nd invocation */
