
FLAGS=-O0 -c -fdump-tree-all -pedantic \
-Wextra -Wconversion -Wall

all:
	@gcc ${FLAGS} ${T}             && \
	rm *.o                         && \
        echo ~~~~~~~~~~~~~~~~~~~~~~~~~ && \
	cat ${T}.*.original |             \
	sed '/    struct/,/return/!d'  && \
	echo
	@# echo ~~~~~~~~~~~~~~~~~~~~~~ && \
	# cat $T.*.gimple

clean:
	@rm -f bubble.c.* t.c.* w.c.* x.c.*
