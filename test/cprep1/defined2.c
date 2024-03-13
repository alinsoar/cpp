
#define X Y
#if defined(X) /* Checks that X is defined, not Y */
OK
#endif


#if defined(Y) /* Checks that X is defined, not Y */
OK
#endif
