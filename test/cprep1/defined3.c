
/* DR #258 */

#define repeat(x) x && x                // Line 1
#if repeat(defined fred)                // Line 2
OK1
#endif

#define forget(x) 0                     // Line 3
#if forget(defined fred)                // Line 4
OK2
#endif

/* 
 * In addition, given the order of events, it is unsuitable to say
 * that a defined X expression is "evaluated". Rather it should be
 * described as a textual substitution.
 * 
 * Proposed Committee Response

 * The standard does not clearly specify what happens in this case, so
 * portable programs should not use these sorts of constructs.
 */
