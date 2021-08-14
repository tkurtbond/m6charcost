;;; sm6troff.scm - Convert Mini-Six Characters in JSON to troff -ms.
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
          (header "")
          (underline ""))
      (when-in-hash (name "Name" character)
	(set! header (format #f "~A" name)))
      (when-in-hash (archetype "Archetype" character)
	(set! header (format #f "~A — ~A" header archetype)))
      (when output-player
        (when-in-hash (player "Player" character)
          (set! header (format #f "~A (~A)" header player))))
      (when #f
        (format #t ".NH 1~%~A~%" header)
        (format #t ".LP~%"))
      (when #t
        (format #t ".sp 1v~%.KS~%.LP~%\\fB\\s[+6]~A\\s0\\fP~%.LP~%" header))
      (when-in-hash (description "Description" character)
        (format #t "~A~%" description)
        (format #t ".LP~%"))
      (when-in-hash (stats "Statistics" character)
	(set! statistics (vector->list stats)))
      ;; This works for absolute skills listed with stats.
      (do-list stat-name statistics
	(unless (hash-table-exists? character stat-name)
	  (die 1 "Missing stat name: ~A" stat-name))
	(let ((stat-value (hash-table-ref character stat-name)))
	  (match-let ((#(stat-dice skills ...) stat-value))
            (format #t "\\fB~a ~a\\fP" stat-name stat-dice)
            (loop for skill in skills
                  for i from 1
                  when (= i 1) do (format #t " — ")
                  when (> i 1) do (format #t ", ")
		  do (begin
		       (match-let ((#(skill-name skill-dice) skill))
                         (format #t "~A ~A" skill-name skill-dice))))))
        (format #t "~%.br~%"))
      (when-in-hash (static "Static" character)
        (format #t "\\fBStatic:\\fP ~A~%.br~%" static))
      (when-in-hash (perks "Perks" character)
	(format #t "\\fBPerks:\\fP ")
	(loop for perk across perks
              for i from 1
              when (> i 1) do (format #t ", ")
	      do (match-let ((#(perk-name perk-dice) perk))
                   (format #t "~A ~A" perk-name perk-dice)))
        (format #t "~%.br~%"))
      (when-in-hash (complications "Complications" character)
        (format #t "\\fBComplications:\\fP ")
        (loop for complication across complications
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" complication))
        (format #t "~%.br~%"))
      (when-in-hash (powers "Powers" character)
        (format #t "\\fBPowers:\\fP ")
        (loop for power across powers
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" power))
        (format #t "~%.br~%"))
      (when-in-hash (gear "Gear" character)
        (format #t "\\fBGear:\\fP ")
        (loop for g across gear
              for i from 1
              when (> i 1) do (format #t ", ")
              do (format #t "~A" g))
        (format #t "~%.br~%"))
      (when-in-hash (hero-points "Hero_Points" character)
        (format #t "\\fBHero Points:\\fP ~A~%.br~%" hero-points))
      (format #t "\\fBWL:\\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □~%")
      (format #t ".KE~%")))

  (define (process-filename filename)
    (let ((characters (with-input-from-file filename read-json)))
      (loop for character across characters
	    for i from 1
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

  (define opts
    (list (args:make-option (g generated) #:none "Output a generated date only if title specified."
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

    ;; Output troff -ms setup.
    (format #t "~A" ".\\\" text width
.nr LL 7i
.\\\" left margin
.nr PO 0.75i
.\\\" top margin
.nr HM 0.75i
.\\\" bottom margin
.nr FM 0.75i
.\\\" header/footer width
.nr LT \\n[LL]
.\\\" point size
.nr PS 10p
.\\\" line height
.nr VS 12p
.\\\" font family: A, BM, H, HN, N, P, T, ZCM
.fam EBGaramond
.\\\" paragraph indent
.nr PI 0m
.\\\" Quote indent
.nr QI 2n
.\\\" interparagraph space
.nr PD 0.5v
.\\\" footnote width
.nr FL \\n[LL]
.\\\" footnote point size
.nr FPS (\\n[PS] - 2000)
.\\\" footnote mode
.nr FF 3
.\\\" footnote length
.nr FL 3.4i
.\\\" color for links (rgb)
.ds PDFHREF.COLOUR   0.35 0.00 0.60
.\\\" border for links (default none)
.ds PDFHREF.BORDER   0 0 0
.\\\" point size difference between heading levels
.nr PSINCR 3p
.\\\" heading level above which point size no longer changes
.nr GROWPS 3
.\\\" page numbers in footer, centered
.rm CH
.ds CF %
.\\\" pdf outline fold level
.nr PDFOUTLINE.FOLDLEVEL 3
.\\\" start out in outline view
.pdfview /PageMode /UseOutlines
.hy
.\\\" ----------------------------------------------------------------------
.\\\" The title looks too small if we're using GROWPS, so adjust its size.
.\\\" 
.de TL
.br
.als TL cov*err-not-again
.rn @AB AB
.rn @AU AU
.rn @AI AI
.di cov*tl-div
.par@reset
.ft B
.nr tkb-psincr (\\\\n[PSINCR]*\\\\n[GROWPS])+2p
.ps +\\\\n[tkb-psincr]u
.vs +3p
.ll (u;\\\\n[LL]*5/6)
.nr cov*n-au 0
.DEVTAG-TL
..
")

    (when output-title
      (format #t ".TL~%~A~%.MC 3.4i 0.2i~%" output-title))
    (when (and output-title output-generated)
      (format #t ".LP~%\\fIGenerated: ~A\\fP~%"
              (date->string (current-date) "~Y-~m-~d ~T (~A, ~e ~B ~Y)")))

    (loop for filename in operands
          for i from 1
          when (> i 1) do (format #t "~%~%~%")
          do (process-filename filename)))
  )
;; end of sm6troff.scm
