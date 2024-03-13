
#define XX 1001 3 AA MM 2002

#if XX

#endif


/* 
 * #if 10?22:33
 * 
 * #endif
 */

int w\
w\
\
w\
w;

  #  in\
cl\
ude   "./w/bu\
bb\
le.h"

/* #include AAA ## BBB                     /\* COMMM *\/ */

/* 
 * #define XX 100
 * 
 * #if defined XX && defined XXX || defined XX
 * 
 * #endif
 */

#include <stdlib.h>

/* 
 * #include <stdio.h>
 * #include <stdlib.h>
 */
/* 
 * #include <ctype.h>
 * #include <string.h>
 * #include <math.h>
 * #include <limits.h>
 */

int ___;

void swap(char ** array, int i, int j)
{
    char * tmp;
  
    tmp = array[i];
    array[i] = array[j];
    array[j] = tmp;
}

int strcmp0(char* s1, char *s2)
{
    e();
    unsigned short i;
    unsigned long l1 = strlen(s1), l2 = strlen(s2);
    for (i=0;(i<l1) || (i<l2);i++) {
        if (s1[i] > s2[i]) {
            return 1;
        }
        else {
            ;
        }
        if (s1[i] < s2[i])
            return 0;
    }
    /* never here */
    exit (2);
}

int compare(char * str1, char * str2)
{
    char *tmp1, *tmp2, *x, *y;
    int result;

    tmp1 = malloc(strlen(str1)+1);
    if (tmp1 == NULL)
    {
        perror("ran out of memory");
        exit(11);
    }

    x = str1;
    y = tmp1;
    while (*x != '\n')
        *y++ = toupper(*x++);
    *y='\0';
  
    tmp2 = malloc(strlen(str2)+1);
    if (tmp2 == NULL)
    {
        perror("ran out of memory");
        exit(22);
    }

    x = str2;
    y = tmp2;
    while (*x != '\n')
        *y++ = toupper(*x++);
    *y='\0';

    /* result = strcmp(tmp1, tmp2); */
    result = strcmp0(tmp1+1, tmp2);
    /* printf("X %d %s %s\n", result, tmp1, tmp2); */
    free(tmp1);
    free(tmp2);
    return result;
}

void bubblesort(char ** array, int len)
{
    int i, j;

    for (i = 0; i < len - 1; i++)
    {
        for (j = 0; j < len - 1; j++)
        {
            if (compare(array[j], array[j+1]) > 0)
                swap(array, j, j+1);
        }
    }
}

void print_array(char**array, unsigned short count)
{
    unsigned i;
    for (i = 0; i < count; i ++)
        printf("%s", array[i]);
    printf("\n");
}

int main(int argc, char *const * argv)
{
    int i, count;
    char ** array;
    char buffer[500];
    FILE * file;

    int ***xx;

    if (argc < 2) exit(0);

    file = fopen(argv[1], "r");
    if (file == NULL)
    {
        perror(argv[1]);
        exit(1);
    }

    array = calloc(10000, sizeof(char *));
    for (i = 0; i < 10000; i++)
    {
        char * x;

        x = fgets(buffer, sizeof(buffer), file);
        if (x == NULL) break;

        array[i] = malloc(strlen(buffer)+1);
        strcpy(array[i], buffer);
    }
    count = i;

    fclose(file);

    print_array(array, count);
    bubblesort(array, count);
    print_array(array, count);

    free(array);
    return 0;
}
