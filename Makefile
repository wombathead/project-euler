LISP = sbcl
TARGET = euler

build: $(TARGET).lisp
	sbcl --load "$<" \
		--eval "(sb-ext:save-lisp-and-die \"run\" :executable t :compression 9 :toplevel 'euler:main)"

compile: $(TARGET).lisp
	$(LISP) -c -q $<

run: $(TARGET).lisp
	$(LISP) --script $(TARGET).lisp
