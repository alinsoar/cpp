 #define M_1(a, b) ((a) + (b))
 #if M_1(1, /* Comment about the first argument.
               Comment about the second argument */ 2) == 3
 OK
 #endif

