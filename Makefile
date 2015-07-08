SRCS=util.scm multimethod.scm proc.scm test.scm

test: $(SRCS)
	gsc -o test -exe $(SRCS)
