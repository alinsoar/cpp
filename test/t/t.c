
enum XX;

struct A {
    char a:N;
    const char b;
    char :3;
    /* 
     * char c;
     * long long int b;
     * long double x;
     */
};

/* 
 * void
 * main()
 * {
 *     printf( "%lu\n", sizeof ( struct A ) );
 *     printf( "%lu\n", sizeof ( long double ) );
 *     printf( "%lu\n", sizeof ( long long int ) );
 *     
 * }
 */

/* 
 * int xx;
 * enum a x;
 * 
 * enum a {
 *   a,a
 * };
 */

/* 
 * typedef int x;
 * enum {
 *   a,a
 * } y;
 * 
 * enum {
 *   a,a
 * } *y;
 * 
 * struct {
 *     int x;
 * } z;
 */

/* float f = 10; */

/* 
 * struct {
 *     int a;
 * } ww;
 * 
 * enum x {
 *   a,
 *       x = 100,
 *       y,
 *       e=4,
 *       f,
 *       
 * };
 * 
 * enum {
 *   b,
 *       c,
 *       f
 * };
 * 
 * enum x y;
 */

/* 
 * enum a {
 *   a=19
 * };
 */

/* 
 * struct a {
 *     int x;
 * } w;
 */

/* 
 * struct b {
 *     int x;
 * } z;
 * 
 * int
 * main()
 * {
 *     /\* struct b; *\/
 *     struct a {
 *         struct b *x;
 *         struct qqqq *q;
 *         struct qqqq *q1;
 *     } w;
 *     int a = w.x->x;
 *     struct b {
 *         int x;
 *     } z;
 *     z.x=a+1+sizeof(struct a);
 *     return sizeof(struct a);
 * }
 */



/* 
 * struct {
 *     int x;
 * };
 * 
 * struct a w;
 * struct yyyy;
 * struct www;
 * union www;
 */

/* 
 * int x, y;
 * 
 * struct {
 *     int a;
 *     int a0;
 *     int x:10;
 *     int :23;
 *     union ee {
 *         struct qq1 {
 *             int x;
 *             struct qq2 {
 *                 int xxxx;
 *             };
 *         } ooo;
 *     };
 * 
 *     struct xxxx;
 *     
 *     enum QQ1 {
 *       a,b,c,
 *     };
 *     enum QQ1 w;
 *     struct qq1 ww;
 * } z, w;
 */

/* 
 * struct {
 *     int b;
 * } w1, z1;
 * 
 * struct QQ {
 *     int c;
 * };
 * 
 * struct qq {
 *     int d;
 * } w2;
 * 
 * struct C {
 *     int e;
 * } w3;
 * 
 * struct {
 *     int a;
 * };
 */

/* void buginf(char, float); */

/* 
 * float buginf(char);
 * 
 * float buginf() {
 *     return;
 * }
 */

/* 
 * void buginf(char, float);
 * void buginf();
 */

/* 
 * int main(int[3],...);
 * int main(int[],...);
 */

/* 
 * int a (int, double a[][3]);
 * int b (int, double a[2][3]);
 */

/* 
 * int arr1[c][b];
 * int (*arr2)[a];
 * 
 * void main() {
 *     
 * };
 * 
 * void x();
 * 
 * ;;
 */

/* 
 * void x(){};
 * void main(int, int);
 */


/* 
 * const float main(void, int) { }
 * const float ()(void, int);
 */

/* const int f(void, signed int b); */

/*
 * struct {
 *     int x,y;
 *     int register t:20;
 * }
 *     const
 *     static
 *     auto
 *     register
 *     inline
 *     volatile
 *         m,* const n[m]
 *     ;
 */


/* int z, w; */

/*
 * register
 * void
 *     main(register int x, float a, ...)
 * {
 *     int x;
 *     int y;
 *     i++;
 *     j++;
 * }
 */

/*
 * int
 * main()
 * {
 *     int x,y;
 *     int z,w;
 *
 * }
 */

/*
 * const
 * typedef int a;
 *
 * a a,b;
 */

/*
 * int
 * const
 * x,y;
 */


/*
 * struct {
 *     int x:10;
 * };
 */

/*
 * inline
 *     static auto
 *     int (*const[]) (unsigned int const, inline long);
 */

/*
 * const
 *     int short   const (x[]);
 */



/*
 * static
 * inline
 *     unsigned  int (* const ** volatile restrict x)[100](const static char, inline int);
 *
 * static inline const int * * const * (* volatile x[10]);
 *
 * static inline const int (*const x)[10];
 *
 * char (*(*x[3])())[5] = (char) 10;
 *
 * char (*(*x[3])())[5];
 *
 * char (*(*x())[])()
 *
 * void (*c)();
 *
 * void*c();
 *
 * int*d[13];
 *
 * int (*d)[13];
 *
 * char ** a;
 */
