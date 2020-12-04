(require 'asdf)
(require 'cmp)

;;; Quiet things down.
;(setf *load-verbose* nil)
;(setf *compile-verbose* nil)
;(setf c::*suppress-compiler-warnings* t)
;(setf c::*suppress-compiler-notes* t)

;;; This flag determines how lisp constants are compiled into the program.
;;; The default scheme does not work well in statically linked libraries
;;; yet.
(setf c::*compile-in-constants* t)

(push (make-pathname :name nil :type nil :version nil
                     :defaults *load-truename*)
      asdf:*central-registry*)


(princ "

Building standalone executable 'lm6'

")

(asdf:load-system :lm6)

(asdf:make-build :lm6
                 :type :program
                 ;; :prologue-code '(require :asdf)
                 ;; :epilogue-code '(ext:quit 0)
                 :epilogue-code '(lm6:main)
                 :move-here "./")

;(ext:quit 0)
