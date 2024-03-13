
/* https://www.gamedev.net/topic/485796-cc-preprocessor-macro-replacement/ */

#define j(x, y) jj(x, y)
#define jj(x, y) x ## y
#define kk(x) x

j(kk(k), kk(k))(l)

