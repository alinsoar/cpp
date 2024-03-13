#define isprint __PRINT_PROPERTY
#define __PRINT_PROPERTY(x) (((x < 0) || (x > 127)) ? 0 : __IS_PRINT[x])
int (__PRINT_PROPERTY)(int);
void f(void)
{
_Bool a_printable = isprint(’a’);
}
