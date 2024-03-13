#define min(X, Y)                               \
   ({                                           \
   typeof (X) __x = (X),                        \
   __y = (Y);                                   \
   (__x < __y) ? __x : __y;                     \
   })
