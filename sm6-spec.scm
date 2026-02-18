;;; sm6.scm - calculate points costs of Mini-Six Characters in JSON.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-2.json
(module sm6 ()
  (import args)
  (import format)
  (import loop)
  (import scheme)
  (import matchable)
  (import medea)
  (import yaml)
  (import simple-loops)
  (import (srfi 13))
  (import (srfi 69))
  (import (chicken base))
  (import (chicken irregex))
  (import (chicken pathname))
  (import (chicken port))
  (import (chicken process-context))

  (define debugging #f)
  (define dbg
    (lambda args
      (cond (debugging
             (format (current-error-port) "~A: dbg: " (program-name))
             (apply format (cons (current-error-port) args))
             (format (current-error-port) "\n")
             (flush-output (current-error-port)))
            (else #f))))

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

  ;; (put 'when-in-alist 'scheme-indent-function 1)
  (define-syntax when-in-alist
    (syntax-rules ()
      ((_ (var key alist) b1 ...)
       (let ((val (assoc key alist)))
       (when val
	 (let ((var (cdr val)))
	   b1 ...))))))

  (define *line-of-equals* (make-string 43 #\=))
  (define *scanner* (irregex "^([0-9]+)[Dd](\\+([1-2]))?$"))
  (define *pips-per-die* 3)
  (define *print-increase* #t) ;set from command line???

  (define (dice-to-cost dice)
    (dbg "dice-to-cost: dice: ~a" dice)
    (let ((m (irregex-match *scanner* dice)))
      (unless m				; can't decide which is better.
	(if #f (die 1 "invalid dice: ~s" dice)
	    (error 'dice-to-cost (format #f "invalid dice: ~s" dice))))
      (let* ((num-dice (string->number (irregex-match-substring m 1)))
	     (num-pips (irregex-match-substring m 3))
	     (num-pips (if num-pips (string->number num-pips) 0)))
	(+ (* num-dice *pips-per-die*)
	   (if num-pips num-pips 0)))))

  (define *default-stat-dice* "12D")
  (define *default-stat-cost* (dice-to-cost *default-stat-dice*))
  (define *default-skill-dice* "7D") ;set from command line or json???
  (define *default-skill-cost* (dice-to-cost *default-skill-dice*))

  (define (cost-to-dice cost)
    (let* ((dice (truncate (/ cost *pips-per-die*)))
           (pips (remainder cost *pips-per-die*))
           ;; If dice is zero, then that number won't have a sign, but
           ;; the dice code should be negative, so add it explicitly.
           (neg (if (and (< cost 0) (= dice 0)) "-" "")))
      (format #f "~A~DD+~D" neg dice (abs pips))))

  (define (calculate-character character)
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
	  (total-stat-cost 0)
	  (total-skill-cost 0)
	  (total-perk-cost 0)
	  (total-skill-and-perk-cost 0))
      (when-in-alist (name "Name" character)
	(format #t "Name: ~A" name))
      (when-in-alist (archetype "Archetype" character)
        (if (assoc "Name" character)
            (format #t " - ~A" archetype)
	    (format #t "~A" archetype)))
      (format #t "~%")
      (when-in-alist (description "Description" character)
        (format #t "      ~A~%" description))
      (when-in-alist (player "Player" character)
	(format #t "Player: ~A~%" player))
      (when-in-alist (stats "Statistics" character)
	(set! statistics stats))
      ;; TODO: Check what form skills are listed, relative or absolute.
      ;; TODO: Check where skills listed, with stats or under 'skills'
      ;; 
      ;; This works for absolute skills listed with stats.
      (dbg "before statistics")
      (do-list stat-name statistics
        (dbg "stat: ~a" stat-name)
	(unless (assoc stat-name character)
	  (die 1 "Missing stat name: ~A" stat-name))
	(let ((stat-value (assoc stat-name character)))
	  (match-let (((stat-name stat-dice skills ...) stat-value))
	  (let ((stat-cost (dice-to-cost stat-dice)))
	    (inc! total-stat-cost stat-cost)
	    (format #t "~30a (~3d points)~%" 
	   	    (format #f "~a: ~a" stat-name stat-dice) stat-cost)
	    (loop for skill in skills
		  do (begin
                       (dbg "skill: ~a" skill)
		       (match-let (((skill-name skill-dice specializations ...) skill))
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
				   relative-cost)
                           ;; Skill specializations!
                           (loop for specilization in specializations
                                 do (begin
                                      (dbg "specialization: ~a" specialization)
                                      (match-let (((spec-name spec-dice) specialization))
                                        (let* ((spec-cost (dice-to-cost spec-dice))
                                               (relative-cost (/ (- spec-cost skill-cost))
                                               (real-relative-cost ( )
                                      ))

                           ))))))))
      (dbg "before perks")
      (when-in-alist (perks "Perks" character)
	(format #t "Perks:~%")
	(loop for perk in perks
	      do (begin
                   (dbg "perk: ~a" perk)
                   (match-let (((perk-name perk-dice) perk))
		     (let ((perk-cost (dice-to-cost perk-dice)))
		       (inc! total-perk-cost perk-cost)
		       (inc! total-skill-and-perk-cost perk-cost)
		       (format #t "    ~26A (~3D points)~%"
			       (format #f "~A: ~A" perk-name perk-dice)
			       perk-cost))))))
      (dbg "before totals")
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

  (define (process-characters characters)
    (loop for character in characters
          for i from 1
          when (> i 1) do (format #t "~A~%" *line-of-equals*)
          do (calculate-character character)))

  (define (process-filename filename)
    (let* ((port (if (string=? filename "-")
                     (current-input-port)
                     (open-input-file filename)))
           (ext (pathname-extension filename))
           (fun (cond ((or use-json (and ext (string=? ext "json")))
                       read-json)
                      ((or use-yaml (and ext (string=? ext "yaml")))
                       yaml-load)
                      (else
                       (if (string=? filename "-")
                           (die 5 "you must specify -y or -j if reading from stdin.")
                           (die 3 "unrecognized format: \"~A\" in filename \"~A\""
                                (if ext ext "") filename))))))
      (process-characters (fun port))))
  
  ;; We want 
  (json-parsers
   `(;; Don't change key to symbol
     (member . ,(lambda (name value) (cons name value)))
     ;; Convert arrays to list
     (array . ,(lambda (a) a))
     ,@(json-parsers)))

  (define use-yaml #f)
  (define use-json #f)

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
        (print "Usage: " (program-name) " [options...] [files...]")
        (newline)
        (print (args:usage +command-line-options+))
        (newline)
        (format #t "Current argv: ~s~%" (argv))))
    (exit 1))

  (define +command-line-options+
    (list (args:make-option
           (d debug)
           #:none "Turn on debugging.  Useful when your input is not correct"
           (set! debugging #t))
          (args:make-option
           (h help) #:none "Display this text."
           (usage))
          (args:make-option (j json) #:none "Assume input file is json."
            (set! use-json #t))
          (args:make-option (y yaml) #:none "Assume input file is yaml."
           (set! use-yaml #t))))

  (receive (options operands)
      (args:parse (command-line-arguments) +command-line-options+)
    (cond ((= (length operands) 0)
           (process-filename "-"))
          (else 
           (loop for filename in operands
                 do (process-filename filename)))))
  )
;; end of sm6.scm
