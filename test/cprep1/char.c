


#if 'z' - '\a' != L'\uf3230101'
A
#if (L'z' - 'a' == 25)
B
#endif
C
#endif
D


#if '\a' - '\a' == 0
E
#if (L'z' - 'a' == 25)
F
#endif
#if (L'z' - 'a' == 20)
G X
#endif
H
#endif
I




