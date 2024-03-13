

10

EMPTY # define

/* 
 * 10u
 * 
 * __x86_64__
 * #if 0x100 + 1 - 267  == -10
 * 
 * #endif
 * 
 * #include <time.h>
 * #include <unistd.h>
 * #include <stdio.h>
 * #include <stdlib.h>
 * #include <stdarg.h>
 * #include <sys/stat.h>
 * #include <string.h>
 */

/* 
 * #define A(x) #x
 * 
 * A("10" 'a\n' .2)
 * 
 * #if 'a'+10==10+'a'
 * 
 * #include "test1.h"
 * 
 * "a\n" * "b" + '\n'
 * 
 * #endif
 */

/* 
 * #include <stdio.h>
 * #include <stdio.h>
 */

/* #define x */

/* 
 * #ifndef x
 * 
 * A
 * 
 * #endif
 * 
 * B
 */

/* 
 * #define a 90
 * 
 * #define d 50
 * 
 * #define MM 100
 */

/* 
 * #define F(x,...) x                              \
 *    __VA_ARGS__                                  \
 *    # __VA_ARGS__  x##x                          \
 *    10 ww MM
 */

/* #define F(x,e,...) x##e e##e e##x x##x */

/* MM */

/* F(a x y,,a c,d) */

/* 
 * mm
 * #define ww 77 88 99 mm
 * #define mm 20 ww
 * #define xx 555
 * 
 * ww
 * 
 * #define a(w,p) p #p x##xx ; xx##p ; p##p ; xx##xx ; mm
 * 
 * #define a(ee, void,q) L## q ## void ## void # ee
 * 100 111 q p q #p #p #q
 * 
 * /\* a(100) *\/
 * 
 * a(100, 200, 300)
 * 
 * #define a(p,q) x ##p q##q q##p p q #q #p
 * 
 * a(2 3 4,)
 */

/* 
 * #define qq(x,y)
 * #define nn yy 100 pp qq
 * #define mm mm nn (33)
 * #define yy # # mm
 * #define pp mm+1
 * yy
 * 
 * #define qq(x,y) x%y+pp
 * #define pp(x,y) x+y+qq
 * #define xx pp(22, 33)(99, 88)(33,44)
 * xx 
 * 
 * 
 * /\* #define X(a) `a` *\/
 * X()
 */

