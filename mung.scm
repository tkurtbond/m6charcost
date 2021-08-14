(import yaml)

(define filename-yaml "/Users/tkb/current/RPG/Mini-Six/charcost/kids-pcs-2.yaml")
(define kids-yaml 
  (call-with-input-file filename-yaml  (lambda (port) (yaml-load port))))

(import (srfi 69))
(import medea)

(json-parsers
 `(;; Don't change key to symbol
   (member . ,(lambda (name value) (cons name value)))
   ,@(json-parsers)))


(define filename-json "/Users/tkb/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-2.json")
(define kids-json (with-input-from-file filename-json read-json))
