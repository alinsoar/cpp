#define i(x) 3
#define a i(yz
#define b )

a b ) ;

/*
* Expands via the following stages:

i(yz b ) ) delimits the argument list before b is expanded
i(yz ) ) the argument to i is the two preprocessing tokens yz )
3
*/
