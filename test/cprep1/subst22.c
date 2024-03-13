#define F(a) a
#define FUNC(a) (a+1)
void f(void)
{
/*
* The preprocessor works successively through the input without
* backing up through previous processed preprocessing tokens.
*/
F(FUNC) FUNC (3); /* final token sequence is FUNC(3+1) */
}
