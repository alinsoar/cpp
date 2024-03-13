
#define ww 11 22 mm
#define mm 33
#define xx 44

#define a(p) p##xx ; \
   xx##p ; p##p ; xx##xx ; mm

#define b(p,q) p##p##q ; .##p ; p##. ; 

#define c(a,b) a##b

a(55)

a(66 77 ww)

b(,)
b(1,)
b(,1)
b(1,1)


c(1 2 ., m 5 6)
