
#define f(a) f(x * (x))
#define g f
#define t(a) a
#define x 3

t(g)(0)

/* 
 * #define f(a) 0
 * #define g f
 * #define t(a) a
 * 
 * t(g)
 */
