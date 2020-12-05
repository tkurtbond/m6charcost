;;; sm6.scm - calculate points costs of Mini-Six Characters in JSON.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-2.json
(module sm6 ()
  (import format)
  (import loop)
  (import scheme)
  (import matchable)
  (import medea)
  (import simple-loops)
  (import srfi-13)
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

  (define-syntax inc!
    (syntax-rules ()
      ((inc! var)
       (set! var (+ var 1)))
      ((inc! var value)
       (set! var (+ var value)))))

  ;; (put 'when-in-hash 'scheme-indent-function 1)
  (define-syntax when-in-hash
    (syntax-rules ()
      ((_ (var key hash-table) b1 ...)
       (when (hash-table-exists? hash-table key)
	 (let ((var (hash-table-ref hash-table key)))
	   b1 ...)))))

  (define *line-of-equals* (make-string 43 #\=))
  (define *scanner* (irregex "^([0-9]+)[Dd](\\+([1-2]))?$"))
  (define *pips-per-die* 3)
  (define *default-stat-dice* "12D")
  (define *default-stat-cost* (dice-to-cost *default-stat-dice*))
  (define *default-skill-dice* "7D") ;set from command line or json???
  (define *default-skill-cost* (dice-to-cost *default-skill-dice*))
  (define *print-increase* #t) ;set from command line???

  (define (dice-to-cost dice)
    (let ((m (irregex-match *scanner* dice)))
      (unless m				; can't decide which is better.
	(if #f (die 1 "invalid dice: ~s" dice)
	    (error 'dice-to-cost (format #f "invalid dice: ~s" dice))))
      (let* ((num-dice (string->number (irregex-match-substring m 1)))
	     (num-pips (irregex-match-substring m 3))
	     (num-pips (if num-pips (string->number num-pips) 0)))
	(+ (* num-dice *pips-per-die*)
	   (if num-pips num-pips 0)))))

  (define (cost-to-dice cost)
    (let ((dice (truncate (/ cost *pips-per-die*)))
	  (pips (modulo cost *pips-per-die*)))
      (format #f "~DD+~D" dice pips)))

  (define (calculate-character character)
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
	  (total-stat-cost 0)
	  (total-skill-cost 0)
	  (total-perk-cost 0)
	  (total-skill-and-perk-cost 0))
      (when-in-hash (name "Name" character)
	(format #t "Name: ~A" name))
      (when-in-hash (archetype "Archetype" character)
	(format #t " - ~A" archetype))
      (format #t "~%")
      (when-in-hash (description "Description" character)
        (format #t "      ~A~%" description))
      (when-in-hash (player "Player" character)
	(format #t "Player: ~A~%" player))
      (when-in-hash (stats "Statistics" character)
	(set! statistics (vector->list stats)))
      ;; TODO: Check what form skills are listed, relative or absolute.
      ;; TODO: Check where skills listed, with stats or under 'skills'
      ;; 
      ;; This works for absolute skills listed with stats.
      (do-list stat-name statistics
	(unless (hash-table-exists? character stat-name)
	  (die 1 "Missing stat name: ~A" stat-name))
	(let ((stat-value (hash-table-ref character stat-name)))
	  (match-let ((#(stat-dice skills ...) stat-value))
	  (let ((stat-cost (dice-to-cost stat-dice)))
	    (inc! total-stat-cost stat-cost)
	    (format #t "~30a (~3d points)~%" 
	   	    (format #f "~a: ~a" stat-name stat-dice) stat-cost)
	    (loop for skill in skills
		  do (begin
		       (match-let ((#(skill-name skill-dice) skill))
			 (let* ((skill-cost (dice-to-cost skill-dice))
				(relative-cost (- skill-cost stat-cost))
				(relative-dice (string-append
                                                "+"
                                                (cost-to-dice relative-cost))))
			   (inc! total-skill-cost relative-cost)
			   (inc! total-skill-and-perk-cost relative-cost)
			   (format #t "    ~19A ~6@A (~3D points)~%"
				   (format #f "~a: ~a" skill-name skill-dice)
				   relative-dice
				   relative-cost)))))))))
      (when-in-hash (perks "Perks" character)
	(format #t "Perks:~%")
	(loop for perk across perks
	      do (match-let ((#(perk-name perk-dice) perk))
		   (let ((perk-cost (dice-to-cost perk-dice)))
		     (inc! total-perk-cost perk-cost)
		     (inc! total-skill-and-perk-cost perk-cost)
		     (format #t "    ~26A (~3D points)~%"
			     (format #f "~A: ~A" perk-name perk-dice)
			     perk-cost)))))
      (let* ((total-stat-dice (cost-to-dice total-stat-cost))
             (total-stat-increase-cost (- total-stat-cost *default-stat-cost*))
             (total-stat-increase-dice (cost-to-dice total-stat-increase-cost))
	     (total-skill-dice (cost-to-dice total-skill-cost))
	     (total-perk-dice (cost-to-dice total-perk-cost))
	     (total-skill-and-perk-dice (cost-to-dice total-skill-and-perk-cost))
             (total-skill-increase-cost (- total-skill-cost *default-skill-cost*))
             (total-skill-increase-dice (cost-to-dice total-skill-increase-cost)))
	(format #t "~23A ~6@A (~3D points)~%"
		"total stat:" total-stat-dice total-stat-cost)
        (when *print-increase*
          (format #t "~23A ~6@A (~3D points)~%"
                  "total stat increase:" total-stat-increase-dice
                  total-stat-increase-cost))
	(format #t "~23A ~6@A (~3D points)~%"
		"total skill:" total-skill-dice total-skill-cost)
        (when *print-increase*
          (format #t "~23A ~6@A (~3D points)~%"
                  "total skill increase:" total-skill-increase-dice
                  total-skill-increase-cost))
        ;; Actually figuring out the cost in character points, not character
        ;; creation balancing points, would be difficult, as you'd have to know
        ;; the original skill values and then run the process forward from
        ;; there to the current values???
	(format #t "~23A ~6@A (~3D points)~%"
		"total perk:" total-perk-dice total-perk-cost)
	(format #t "~23A ~6@A (~3D points)~%"
		"total (skill + perk):"
		total-skill-and-perk-dice total-skill-and-perk-cost)
	(let* ((total-cost (+ total-stat-cost total-skill-cost
			      total-perk-cost))
	       (total-dice (cost-to-dice total-cost)))
	  (format #t "~23A ~6@A (~3D points)~%" 
		  "total:" total-dice total-cost)))))

  (define (process-filename filename)
    (let ((characters (with-input-from-file filename read-json)))
      (loop for character across characters
	    for i from 1
	    when (> i 1) do (format #t "~A~%" *line-of-equals*)
	    do (calculate-character character))))

  ;; We want 
  (json-parsers
   `(;; Don't change key to symbol
     (member . ,(lambda (name value) (cons name value)))
     ;; Convert objects from alist to hash table
     (object . ,(lambda (object) (alist->hash-table object)))
     ,@(json-parsers)))

  #|(match (command-line-arguments)
    (()
     (die 1 "No file specified"))
    ((filename) (process-filename filename))
    ((filename . others) 
     (die 1 "unexpected command line arguments: ~A" others)))|#

  (when (= (length (command-line-arguments)) 0)
    (die 1 "No filenames specified!"))
  (loop for filename in (command-line-arguments)
        for i from 1
        when (> i 1) do (format #t "~%~A~%~%" (make-string 43 #\@))
        do (process-filename filename))
  )
;; end of sm6.scm
