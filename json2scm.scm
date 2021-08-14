#!/usr/bin/env -S csi -s

(import medea)
(import srfi-69)
(import loop)
(import (chicken pretty-print))
(import (chicken process-context))

(json-parsers
 `(;; Don't change key to symbol
   (member . ,(lambda (name value) (cons name value)))
   ;; Convert objects from alist to hash table
   ;;(object . ,(lambda (object) (alist->hash-table object)))
   ;; Convert objects from array to list
   (array . ,(lambda (a) a))
   ,@(json-parsers)))

(define (process-filename filename)
  (let ((json (with-input-from-file filename read-json)))
    (pp json)
    (newline)))

(loop for filename in (command-line-arguments) do (process-filename filename))

