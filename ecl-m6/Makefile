m6: make.lisp lm6.lisp
	ecl --load make.lisp --eval '(ext:quit 0)'

run: m6 kids-pcs-2.yaml
	./m6 kids-pcs-2.yaml

ql:
	ecl --eval '(progn (ql:quickload :alexandria) (ql:quickload :cl-ppcre) (ql:quickload :cl-yaml) (ql:quickload :trivia) (ext:quit 0))'

clean:
	-rm lm6
