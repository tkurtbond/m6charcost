;;; sm6rst.scm - Convert Mini-Six Characters in JSON to ReStructuredText.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-3.json
(module sm6print ()
  (import args)
  (import format)
  (import loop)
  (import scheme)
  (import matchable)
  (import medea)
  (import simple-loops)
  (import srfi-13)
  (import srfi-19)
  (import srfi-69)
  (import (chicken base))
  (import (chicken irregex))
  (import (chicken port))
  (import (chicken process-context))

  (define (die status . args)
    (format (current-error-port) "~A: fatal error: " (program-name))
    (apply format (cons (current-error-port) args))
    (format (current-error-port) "\n")
    (exit status))

  ;; (put 'when-in-hash 'scheme-indent-function 1)
  (define-syntax when-in-hash
    (syntax-rules ()
      ((_ (var key hash-table) b1 ...)
       (when (hash-table-exists? hash-table key)
	 (let ((var (hash-table-ref hash-table key)))
	   b1 ...)))))

  (define (print-character character)
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
          (header ""))
      (when-in-hash (name "Name" character)
	(set! header (format #f "~A" name)))
      ;; Using an Unicode EM DASH makes the underline longer.  utf8 egg???
      (when-in-hash (archetype "Archetype" character)
	(set! header (format #f "~A — ~A" header archetype)))
      (when-in-hash (number "Number" character)
	(set! header (format #f "~A ×~A" header number)))
      (when output-player
        (when-in-hash (player "Player" character)
          (set! header (format #f "~A (~A)" header player))))
      (format #t "~A~%" header)
      (format #t "~A~%~%" (make-string (string-length header) #\=))
      (when-in-hash (description "Description" character)
        (format #t "~A~%~%" description))
      (when-in-hash (stats "Statistics" character)
	(set! statistics (vector->list stats)))
      ;; This works for absolute skills listed with stats.
      (do-list stat-name statistics
	(unless (hash-table-exists? character stat-name)
	  (die 1 "Missing stat name: ~A" stat-name))
	(let ((stat-value (hash-table-ref character stat-name)))
	  (match-let ((#(stat-dice skills ...) stat-value))
            (format #t "| **~a ~a**" stat-name stat-dice)
            (loop for skill in skills
                  for i from 1
                  when (= i 1) do (format #t " — ")
                  when (> i 1) do (format #t ", ")
		  do (begin
		       (match-let ((#(skill-name skill-dice) skill))
                         (format #t "~A ~A" skill-name skill-dice))))))
        (format #t "~%"))
      (when-in-hash (static "Static" character)
        (format #t "| **Static:** ~A~%" static))
      (when-in-hash (move "Move" character)
        (format #t "| **Move:** ~A~%" move))
      (when-in-hash (perks "Perks" character)
	(format #t "| **Perks:** ")
	(loop for perk across perks
              for i from 1
              when (> i 1) do (format #t ", ")
	      do (match-let ((#(perk-name perk-dice) perk))
                   (format #t "~A ~A" perk-name perk-dice)))
        (format #t "~%"))
      (when-in-hash (complications "Complications" character)
        (format #t "| **Complications:** ")
        (loop for complication across complications
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" complication))
        (format #t "~%"))
      (when-in-hash (powers "Powers" character)
        (format #t "| **Powers:** ")
        (loop for power across powers
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" power))
        (format #t "~%"))
      (when-in-hash (gear "Gear" character)
        (format #t "| **Gear:** ")
        (loop for g across gear
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" g))
        (format #t "~%"))
      (when-in-hash (hero-points "Hero_Points" character)
        (format #t "| **Hero Points:** ~A~%" hero-points))
      (cond (output-breachworld
             (format #t "| **WL:** D:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □~%")
             (format #t "| **SL:** S:1–8 □ SS:9+ □~%"))
            (else 
             (format #t "| **WL:** S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □~%")))))

  (define (process-filename filename)
    (let ((characters (with-input-from-file filename read-json)))
      (loop for character across characters
	    for i from 1
	    when (> i 1) do (format #t "~%~%")
	    do (print-character character))))

  ;; We want 
  (json-parsers
   `(;; Don't change key to symbol
     (member . ,(lambda (name value) (cons name value)))
     ;; Convert objects from alist to hash table
     (object . ,(lambda (object) (alist->hash-table object)))
     ,@(json-parsers)))

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
	(print "Usage: " (program-name) " [options...] [files...]")
	(newline)
	(print (args:usage opts))
	(format #t "Current argv: ~s~%" (argv))))
    (exit 1))


  (define output-generated #f)
  (define output-player #t)
  (define output-title #f)
  (define output-breachworld #f)

  (define opts
    (list (args:make-option (b breachworld) #:none "Output different wound statuses for Breachworld"
                            (set! output-generated (not output-generated)))
          (args:make-option (g generated) #:none "Output a generated date only if title specified."
			    (set! output-generated (not output-generated)))
          (args:make-option (p player) #:none "Toggle player name output (default ON)"
			    (set! output-player (not output-player)))
          (args:make-option (t title) #:required "Set title to output."
                            (set! output-title arg))))
	  

  (receive (options operands)
      (args:parse (command-line-arguments)
		  opts)
    (when #f
      (format #t "output-player: ~A~%output-title: ~A~%" output-player
              output-title)
      (format #t "options: ~S~%operands: ~S~%" options operands))

    (when (= (length operands) 0)
      (die 1 "No filenames specified!"))
    (when output-title
      (format #t "~A~%~A~%~%~%" output-title
              (make-string (string-length output-title) #\@))
      (when output-generated
        (format #t "*Generated: ~A*~%~%~%"
                (date->string (current-date) "~Y-~m-~d ~T (~A, ~e ~B ~Y)"))))

    (loop for filename in operands
          for i from 1
          when (> i 1) do (format #t "~%~%~%")
          do (process-filename filename)))
  )
;; end of sm6rst.scm
