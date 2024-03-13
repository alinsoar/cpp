#define FUNC(x) x * glob_1

extern int glob_1,
glob_2;
void f(void)
{
    int loc = FUNC(2 + glob_2);
}
