
#define SUM a + b
extern int glob;
void f(void)
{
    int loc = glob * SUM;
}
