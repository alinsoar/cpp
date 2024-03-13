#define L_PAREN (
#define FM1(a) a
#define FM2(b) b 23)
void f1(void)
{
FM2(FM1 L_PAREN);
}
void f2(void)
{
FM2(FM1(L_PAREN));
}
#define FM3(c, d) c d 23)
void f3(void)
{
FM3(FM1, L_PAREN);
}
