
#define INIT c=0; d=0;
extern int glob;
void f(void)
{
    if (glob == 0)
        INIT;
}
