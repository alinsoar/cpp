
#if DEF
#define FOO defined
#else
#define FOO 1 +
#endif

#if FOO BAR
#endif

#define PAREN_PLUS ) + 1

#if defined(X PAREN_PLUS
#endif

#define M(a) defined(a)

#if M(X)

#endif
