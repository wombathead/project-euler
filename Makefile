TARGET = euler

lisp: $(TARGET).lisp
	clisp -c -q $<

run-lisp:
	clisp $(TARGET).fas
