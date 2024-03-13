
#define mk_sys_hdr(name) < ## name ## >
#if BUG_FIX
#define VERSION 2a /* works because pp-numbers include alphabetics */
#else
#define VERSION 2
#endif
#define add_quotes(a) # a
#define mk_str(str, ver) add_quotes(str ## ver)
#include mk_str(Version, VERSION)
