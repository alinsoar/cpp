#define PARAM param
#define F(param) param + PARAM
int glob = F(1); /* Expands to 1+param, rather than 1+1 */
