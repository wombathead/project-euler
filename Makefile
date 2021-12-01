LISP = sbcl
TARGET = euler

compile: $(TARGET).lisp
	$(LISP) -c -q $<

run: $(TARGET).lisp
	$(LISP) --script $(TARGET).lisp
