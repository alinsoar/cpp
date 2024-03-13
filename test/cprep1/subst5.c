#define mkstr(a) # a
char*p_1=mkstr("mnp\a");/* Assigns "\"mnp\\a\"" */
char*p_2=mkstr(000);/* Assigns "000" */
char*p_3=mkstr(0);/* Assigns "0" */
char*p_4=mkstr(0004);/* Assigns "0004" */
char*p_5=mkstr(0.001E+000);/* Assigns "0.001E+000" */
char*p_6=mkstr(x\
 yz);                 /* No new-line in created string, i.e., "xyz" */
char*p_7 = mkstr(O
 K);                                    /* Assigns "O K" */
