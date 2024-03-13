#define MK_STR(a) #a
#define GLUE(a, b) a ## b
char *p_1 = MK_STR(GLUE(1, 2));
#define STR_GLUE(a, b) MK_STR(a ## b)
char *p_2 = STR_GLUE(1, 2);
