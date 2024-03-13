/* file_1.c */
#define __CONCAT__(_A, _B) _A ## _B
#define __CONCAT_U__(_A) _A ## u
#define ULONG32_C(__c) __CONCAT__(__CONCAT_U__(__c), l)
#define ULONG32_MAX ULONG32_C(4294967295)

unsigned long max = ULONG32_MAX;

/* file_2.c */
#define __XXCONCAT__(_A, _B) _A ## _B
#define __CONCAT__(_A, _B) __XXCONCAT__(_A, _B)
#define __XCONCAT_U__(_A) _A ## u
#define __CONCAT_U__(_A) __XCONCAT_U__(_A)
#define ULONG32_C(__c) __CONCAT__(__CONCAT_U__(__c), l)
#define ULONG32_MAX ULONG32_C(4294967295)

unsigned long max = ULONG32_MAX;
