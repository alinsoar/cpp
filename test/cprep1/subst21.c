static int M_0 = 0;
#define M_0(x) M_ ## x
#define M_1(x) x + M_0(0)
#define M_2(x) x + M_1(1)
#define M_3(x) x + M_2(2)
#define M_4(x) x + M_3(3)
#define M_5(x) x + M_4(4)
int f_1(void)
{
return M_0(1)(2)(3)(4)(5); /* Expands to:
2 + M_0(3)(4)(5)
or
2 + M_0(0)(3)(4)(5) */
}
int f_2(void)
{
return M_0(5)(4)(3)(2)(1); /* Expands to: 4 + 4 + 3 + 2 + 1 + M_0(3)(2)(1) */
}
