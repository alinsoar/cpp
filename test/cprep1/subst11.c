#define eprintf(format, args...) \
   fprintf (stderr, format , ## args)

/* The invocation */
eprintf("failure!\n");
/* is expanded to:
*
*
 fprintf(stderr, "failure!\n");
*
* rather than to:
*
*
 fprintf(stderr, "failure!\n" , );
*/
