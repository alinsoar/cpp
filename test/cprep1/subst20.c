
extern int M_1,
M_2;
#define M_1 M_2
#define M_2 M_1
void f(void)
{
M_1 = M_2; /* Macros do not alter the behavior, i.e., M_2 is still
            * assigned to * M_1 */
}
 #define short static short
 short si; /* Expands to static short si; */
