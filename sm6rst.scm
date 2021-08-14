;;; sm6rst.scm - Convert Mini-Six Characters in JSON to ReStructuredText.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-3.json
(module sm6rst ()
  (import args)
  (import format)
  (import loop)
  (import scheme)
  (import matchable)
  (import medea)
  (import yaml)
  (import simple-loops)
  (import (srfi 1))
  (import (srfi 13))
  (import (srfi 19))
  (import (srfi 69))
  (import (chicken base))
  (import (chicken irregex))
  (import (chicken pathname))
  (import (chicken port))
  (import (chicken process-context))
  (import (chicken sort))
  (import (chicken string))

  (define (die status . args)
    (format (current-error-port) "~A: fatal error: " (program-name))
    (apply format (cons (current-error-port) args))
    (format (current-error-port) "\n")
    (exit status))

  (define (dbg . args)
    (format (current-error-port) "~A: dbg: " (program-name))
    (apply format (cons (current-error-port) args)))
  
  ;; (put 'when-in-alist 'scheme-indent-function 1)
  (define-syntax when-in-alist
    (syntax-rules ()
      ((_ (var key alist) b1 ...)
       (let ((val (assoc key alist)))
         (when val
	   (let ((var (cdr val)))
	     b1 ...))))))

  (define (output-notes-line line)
    (define (iter substitutes)
      (if (null? substitutes)
          #f
          (let* ((substitute (car substitutes))
                 (rx `(bol (+ ,(car substitute)) eol))
                 (ch (cdr substitute)))
            (if (irregex-match rx line)
                (make-string (string-length line) ch)
                (iter (cdr substitutes))))))
    (let ((new-line (if substitutes (iter substitutes) line)))
      (format #t "~A~%" (cond (substitutes
                               (if new-line new-line line))
                              (else line)))))

  (define (print-npc-attributes-and-skills character statistics)
    (format #t "| **Attibutes:** ")
    (define skills '())
    (loop for name in statistics for i from 1
          do (begin
               (let ((stat (assoc name character)))
                 (unless stat
	           (die 1 "Missing stat name: ~A" name))
                 (when (> i 1) (format #t ", "))
                 (match-let (((stat-name stat-dice stat-skills ...)
                              stat))
                   ;; Don't need bold, since we have Attributes.
                   (format #t "~A ~A" stat-name stat-dice)
                   (set! skills (append skills stat-skills))))))
    (format #t "~%")
    (set! skills (sort skills (lambda (x y) (string<? (car x) (car y)))))
    (format #t "| **Skills:** ")
    (loop for skill in skills for i from 1
          do (match-let (((skill-name skill-dice) skill))
               (when (> i 1) (format #t ", "))
               (format #t "~A ~A" skill-name skill-dice)))
    (format #t "~%"))

  (define (print-pc-attributes-and-skills character statistics)
    (do-list stat-name statistics
      (unless (assoc stat-name character)
	(die 1 "Missing stat name: ~A" stat-name))
      (let ((stat-value (assoc stat-name character)))
	(match-let (((stat-name stat-dice skills ...) stat-value))
          (when sort-skills (set! skills (sort skills (lambda (x y)
                                                        (string<? (car x)
                                                                  (car y))))))
          (format #t "| **~a ~a**" stat-name stat-dice)
          (loop for skill in skills
                for i from 1
                when (= i 1) do (format #t " — ")
                when (> i 1) do (format #t ", ")
		do (match-let (((skill-name skill-dice) skill))
                     (format #t "~A ~A" skill-name skill-dice)))))
      (format #t "~%")))

  (define blanks-rx (string->irregex "[\t ]*"))

  (define (print-character character)
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
          (header "")
          (outer-name #f))
      (when-in-alist (name "Name" character)
	(set! header (format #f "~A" name))
        (set! outer-name name))
      ;; Using an Unicode EM DASH makes the underline longer.  utf8 egg???
      ;; No, use a hypen-minus, because it appears in the PDF outline, and
      ;; groff doesn't get non-ascii characters right in that.
      (when-in-alist (archetype "Archetype" character)
	(set! header (format #f "~A - ~A" header archetype)))
      (when-in-alist (number "Number" character)
	(set! header (format #f "~A ×~A" header number)))
      (when output-player-name
        (when-in-alist (player "Player" character)
          (set! header (format #f "~A (~A)" header player))))
      (format #t "~A~%" header)
      (format #t "~A~%~%" (make-string (string-length header) underline))
      (when-in-alist (quote "Quote" character)
        (format #t "*“~A”*~%~%" quote))
      (when-in-alist (description "Description" character)
        (format #t "~A~%~%" description))
      (when-in-alist (scale "Scale" character)
        (format #t "| **Scale:** ~A~%" scale))
      (when-in-alist (stats "Statistics" character)
	(set! statistics stats))
      ;; This works for absolute skills listed with stats.
      (if (and (or output-npc-format (assoc "NPC" character))
               (not output-player-format))
          (print-npc-attributes-and-skills character statistics)
          (print-pc-attributes-and-skills character statistics))
      (when-in-alist (perks "Perks" character)
        (when (> (length perks) 0)
	  (format #t "| **Perks:** ")
	  (loop for perk in perks
                for i from 1
                when (> i 1) do (format #t ", ")
	        do (match-let (((perk-name perk-dice) perk))
                     ;; we don't care what it cost since we're not
                     ;; calculating costs.
                     (format #t "~A" perk-name)))
          (format #t "~%")))
      (when-in-alist (complications "Complications" character)
        (when (> (length complications) 0)
          (format #t "| **Complications:** ")
          (loop for complication in complications
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" complication))
          (format #t "~%")))
      ;; Powers and Spells come (almost) right after Perks, because that's
      ;; where Sorcerer will be.
      (when-in-alist (powers "Powers" character)
        (when (> (length powers) 0)
          (format #t "| **Powers:** ")
          (loop for power in powers
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" power))
          (format #t "~%")))
      (when-in-alist (spells "Spells" character)
        (when (> (length spells) 0)
          (format #t "| **Spells:** ")
          (loop for spell in spells
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" spell))
          (format #t "~%")))
      (when-in-alist (gear "Gear" character)
        (when (> (length gear) 0)
          (format #t "| **Gear:** ")
          (loop for g in gear
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" g))
          (format #t "~%")))
      (when-in-alist (statics "Static" character)
        (cond ((list? statics)
               (format #t "| **Static:** ")
               (loop for static in statics for i from 1
                     when (> i 1) do (format #t "; ")
                     do (match static
                          ((static-name static-value) 
                           (format #t "~A ~A" static-name static-value))
                          ((? string? static)
                           (format #t "~A" static))))
               (format #t "~%"))
              (else (format #t "| **Static:** ~A~%" statics))))
      (when-in-alist (defenses "Defenses" character)
        (cond ((list? defenses)
               (format #t "| **Defenses:** ")
               (loop for defense in defenses for i from 1
                     when (> i 1) do (format #t "; ")
                     do (match defense
                          ((defense-name defense-value)
                           (format #t "~A ~A" defense-name defense-value))
                          ((? string? defense)
                           (format #t "~A" defense))))
               (format #t "~%"))
              (else (format #t "| **Defenses:** ~A~%" defenses))))
      (when-in-alist (move "Move" character)
        (format #t "| **Move:** ~A~%" move))
      (when-in-alist (melee "Melee" character)
        (when (> (length melee) 0)
          (format #t "| **Melee:** ")
          (loop for weapon in melee
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%")))
      (when-in-alist (ranged "Ranged" character)
        (when (> (length ranged) 0)
          (format #t "| **Ranged:** ")
          (loop for weapon in ranged
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%")))
      (when-in-alist (natural-armor "Natural Armor" character)
        (when (> (length natural-armor) 0)
          (format #t "| **Natural Armor:** ")
          (loop for armor in natural-armor
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" armor))
          (format #t "~%")))
      (when-in-alist (natural-weapons "Natural Weapons" character)
        (when (> (length natural-weapons) 0)
          (format #t "| **Natural Weapons:** ")
          (loop for weapon in natural-weapons
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%")))
      (when-in-alist (special-defenses "Special Defenses" character)
        (when (> (length special-defenses) 0)
          (format #t "| **Special Defenses:** ")
          (loop for defense in special-defenses
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" defense))
          (format #t "~%")))
      (when-in-alist (special-abilities "Special Abilities" character)
        (when (> (length special-abilities) 0)
          (format #t "| **Special Abilities:** ")
          (loop for ability in special-abilities
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" ability))
          (format #t "~%")))
      (when-in-alist (hero-points "Hero_Points" character)
        (format #t "| **Hero Points:** ~A~%" hero-points))
      (when-in-alist (fate-points "Fate_Points" character)
        (format #t "| **Fate Points:** ~A~%" fate-points))
      (when-in-alist (force-points "Force_Points" character)
        (format #t "| **Force Points:** ~A~%" force-points))
      (cond (output-breachworld
             (format #t "| **WL:** D:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □~%")
             (format #t "| **SL:** S:1–8 □ SS:9+ □~%"))
            (else 
             (format #t "| **WL:** S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □~%")))
      ;; This is very output format dependent: .eps for groff -ms and
      ;; pandoc -w ms, .png for pandoc -w html.
      (when-in-alist (image "Image" character)
        (format #t "~%.. image:: ~A~%" image)
        (when outer-name 
          (format #t   "   :alt: ~A~%" outer-name)))
      (when output-notes
        (when-in-alist (notes "Notes" character)
          (cond
           ((list? notes)
            (when (> (length notes) 0)
              (format #t "~%")
              (loop for line in notes
                    do (output-notes-line line))))
           ((string? notes)
            (format #t "~%")
            (let* ((lines (string-split notes "\n" #t))
                   (lines (if (irregex-match blanks-rx (last lines))
                              (drop-right lines 1)
                              lines)))
              (loop for line in lines
                    do (output-notes-line line))))
           (else
            (die 5 "unrecognized Notes: not a list or string: ~S~%"
                 notes)))))))

  (define (process-characters characters)
    (loop for character in characters
          for i from 1
          when (> i 1) do (format #t "~%")
          do (print-character character)))

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

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
	(print "Usage: " (program-name) " [options...] [files...]")
	(newline)
	(print (args:usage opts))
	(format #t "Current argv: ~s~%" (argv))))
    (exit 1))

  (define output-breachworld #f)
  (define output-generated #f)
  (define output-notes #t)
  (define output-npc-format #f)
  (define output-player-format #f)
  (define output-player-name #t)
  (define output-title #f)
  (define sort-skills #t)
  (define use-yaml #f)
  (define use-json #f)

  ;;(define chunk #f)
  (define debug #f)
  (define underline #\-)
  (define substitutes #f)

  (define opts
    (list (args:make-option
           (b breachworld) #:none "Output different wound track for Breachworld."
           (set! output-breachworld (not output-breachworld)))
          ;;(args:make-option (c chunk) #:none "Chunk output into separate files."
          ;; (set! chunk #t))
          (args:make-option
           (d debug) #:none "Output character for debugging"
           (set! debug (not debug)))
          (args:make-option
           (g generated) #:none
           "Output a generated date only if title specified."
           (set! output-generated (not output-generated)))
	  (args:make-option
           (h help) #:none "Display this text"
           (usage))
          (args:make-option
           (j json) #:none "Assume input file is json."
           (set! use-json #t))
          (args:make-option
           (n npc-format) #:none "Toggle NPC output format (default OFF)"
	   (set! output-npc-format (not output-npc-format)))
          (args:make-option
           (N notes) #:none "Toggle Notes output (default ON)"
	   (set! output-notes (not output-notes)))
          (args:make-option
           (P player-format) #:none
           (string-append
            "Toggle output in player format only (default OFF)\n"
            "                          When this is set NEVER output in NPC format,\n"
            "                          overriding presence of NPC key in character.")
           (set! output-player-format (not output-player-format)))
          (args:make-option
           (p player-name) #:none "Toggle player name output (default ON)"
           (set! output-player-name (not output-player-name)))
          (args:make-option
           (S sort-skills) #:none
           "Toggle sort skills in player format (default ON)"
           (set! sort-skills (not sort-skills)))
          (args:make-option
           (s substitute) #:required 
           (string-append
            "Substitute a whole line of the first character of ARG\n"
            "                          with the second character of ARG throughout the\n"
            "                          \"Notes:\" field.  Can be specified multiple times\n"
            "                          to substitute multiple types of lines.")
           ;; sm6rst -u @ -s -. -s '~#' test-files/test.json 
           (assert (= (string-length arg) 2))
           (let ((new (cons (irregex (string-append
                                      "^" (substring arg 0 1) "+" "$"))
                            (string-ref arg 1))))
             (set! substitutes (if substitutes (cons new substitutes)
                                   (list new)))))
          (args:make-option
           (t title) #:required "Set title to output."
           (set! output-title arg))
          (args:make-option
           (u underline) #:required "Set character to use for underlining the header."
           (set! underline (string-ref arg 0)))
          (args:make-option
           (y yaml) #:none "Assume input file is yaml."
           (set! use-yaml #t))))

  (receive (options operands)
      (args:parse (command-line-arguments) opts)

    (when output-title
      (format #t "~A~%~A~%~%~%" output-title
              (make-string (string-length output-title) #\@))
      (when output-generated
        (format #t "*Generated: ~A*~%~%~%"
                (date->string (current-date) "~Y-~m-~d ~T (~A, ~e ~B ~Y)"))))

    (cond ((= (length operands) 0)
           (process-filename "-"))
          (else 
           (loop for filename in operands
                 do (process-filename filename)))))
  )
;; end of sm6rst.scm
