
/*
* The following are constraint violations.
*/
#define __VA_ARGS__
#define jparks __VA_ARGS__
#define jparks(__VA_ARGS__)
#define jparks(__VA_ARGS__, ...) __VA_ARGS__
#define jparks(x) x
jparks(__VA_ARGS__)
#define jparks(x, ...) x
jparks(__VA_ARGS__,1)
/*
* The following break the spirit, if not the wording
* of this constraint.
*/
#define jparks(x, y) x##y
jparks(__VA, _ARGS__)
#define jparks(x, y, ...) x##y
jparks(__VA, _ARGS__, 1)
