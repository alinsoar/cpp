

enum {
  a,
      b=100+sizeof(enum A),
      c =  sizeof (
      struct {
          int a;
          union {
              int x;
              struct {
                  int t;
              } y;
          } b;
      }
      )+1,
      d
};

/* 
 * void
 * f(struct X {
 *     union {
 *         int aa;
 *         char *x;
 *     };
 *     int bb;
 * } x, int y)
 * {
 *     printf("%lu\n", sizeof(x));
 * }
 */

/* int x = 1+sizeof sizeof sizeof sizeof (struct A); */

/* 
 * struct A {
 *     int a;
 * } q = {
 *   .w = 100,
 *       .u = 200
 * };
 * 
 * int i = sizeof
 * (
 * struct A {
 *     int a;
 * }
 * );
 */


/* 
 * void
 * main()
 * {
 *     ;
 * }
 */

