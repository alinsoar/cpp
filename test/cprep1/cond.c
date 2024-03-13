
#define C 0x1100u
#define INT_BITS 32

#define TOP_BYTE (C << (INT_BITS-8))

#if TOP_BYTE == 0
/* ... */
#endif

void f(void)
{
    if (TOP_BYTE == 0)
        /* ... */ ;
}
