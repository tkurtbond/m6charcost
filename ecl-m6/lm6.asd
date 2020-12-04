(defsystem #:lm6
  ;; This should just include #:uiop, but apparently there is a bug.
  :depends-on (#:asdf #:alexandria #:cl-ppcre #:cl-yaml #:trivia)
  :components ((:file "lm6")))
