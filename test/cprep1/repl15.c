#define FUNC(a) (a+1)
void f(void)
{
(FUNC)(3);
}
/* Final token sequence is (FUNC)(3) */

