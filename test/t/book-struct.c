
struct X {
    restrict char a[5];                 /* 5 */
    const short s1,s2;                  /* 6 */
    unsigned code:3, used:1;            /* 10 */
    unsigned:26;                        /* 10 */
    volatile int amt:7, last;           /* 16 20 */
    short restrict id;                  /* 24 */
} x;

/* 
 * void
 * main()
 * {
 *     printf("%lu\n", sizeof(struct {int x;int y;}));
 * }
 */
