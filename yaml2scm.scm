#!/usr/bin/env -S csi -s

(import yaml)
(import loop)
(import (chicken pretty-print))
(import (chicken process-context))

(define (process-filename filename)
  (call-with-input-file filename (lambda (port)
                                   (let ((y (yaml-load port)))
                                     (pp y)
                                     (newline)))))

(loop for filename in (command-line-arguments) do (process-filename filename))
