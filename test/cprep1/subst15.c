#define GLUE_3(x, y, z) x ## y ## z
GLUE_3(>, >, =)
 /* Behavior defined for all orders of evaluation.
 */
GLUE_3(1, . , e10) /* Behavior only defined if left most ## performed first. */
/* There are no cases where the behavior is only defined if right most ## performed first. */
