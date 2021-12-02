LISP = sbcl
TARGET = euler

build: $(TARGET).lisp
	sbcl --load "$<" \
		--eval "(sb-ext:save-lisp-and-die \"$(TARGET)\" :executable t :toplevel 'main)"

compile: $(TARGET).lisp
	$(LISP) -c -q $<

run: $(TARGET).lisp
	$(LISP) --script $(TARGET).lisp
