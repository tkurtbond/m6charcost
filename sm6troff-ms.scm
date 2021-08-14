;;; sm6troff.scm - Convert Mini-Six Characters in JSON to troff -ms.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-3.json
(module sm6troff-ms ()
  (import args)
  (import format)
  (import loop)
  (import scheme)
  (import matchable)
  (import miscmacros)
  (import medea)
  (import yaml)
  (import simple-loops)
  (import (srfi 13))
  (import (srfi 19))
  (import (srfi 69))
  (import (chicken base))
  (import (chicken irregex))
  (import (chicken pathname))
  (import (chicken port))
  (import (chicken process-context))
  (import (chicken sort))

  (define (die status . args)
    (format (current-error-port) "~A: fatal error: " (program-name))
    (apply format (cons (current-error-port) args))
    (format (current-error-port) "\n")
    (exit status))

  ;; (put 'when-in-alist 'scheme-indent-function 1)
  (define-syntax when-in-alist
    (syntax-rules ()
      ((_ (var key alist) b1 ...)
       (let ((val (assoc key alist)))
         (when val
	   (let ((var (cdr val)))
	     b1 ...))))))

  (define (print-npc-attributes-and-skills character statistics)
    (format #t "\\fBAttibutes:\\fP ")
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
    (format #t "~%.br~%")
    (set! skills (sort skills (lambda (x y) (string<? (car x) (car y)))))
    (format #t "\\fBSkills:\\fP ")
    (loop for skill in skills for i from 1
          do (match-let (((skill-name skill-dice) skill))
               (when (> i 1) (format #t ", "))
               (format #t "~A ~A" skill-name skill-dice)))
    (format #t "~%.br~%"))

  (define (print-pc-attributes-and-skills character statistics)
    (do-list stat-name statistics
      (unless (assoc stat-name character)
	(die 1 "Missing stat name: ~A" stat-name))
      (let ((stat-value (assoc stat-name character)))
	(match-let (((stat-name stat-dice skills ...) stat-value))
          (when sort-skills (set! skills (sort skills (lambda (x y)
                                                        (string<? (car x)
                                                                  (car y))))))
          (format #t "\\fB~a ~a\\fP" stat-name stat-dice)
          (loop for skill in skills
                for i from 1
                when (= i 1) do (format #t " — ")
                when (> i 1) do (format #t ", ")
		do (match-let (((skill-name skill-dice) skill))
                       (format #t "~A ~A" skill-name skill-dice)))))
      (format #t "~%.br~%")))


  (define (print-character character)
    ;; No need for outer-name here, because even if we implement "Image"
    ;; because .PSPIC doesn't have an alt.
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
          (header "")
          (underline ""))
      (when-in-alist (name "Name" character)
	(set! header (format #f "~A" name)))
      (when-in-alist (archetype "Archetype" character)
	(set! header (format #f "~A - ~A" header archetype)))
      (when-in-alist (number "Number" character)
        ;; Using x instead of × because groff can't handle UTF-8 in
        ;; PDF outlines.
        (set! header (format #f "~A x~A" header number)))
      (when output-player-name
        (when-in-alist (player "Player" character)
          (set! header (format #f "~A (~A)" header player))))
      (cond (output-header
             (format #t ".~A ~D~%~A~%"
                     (if output-number-header "NH" "SH")
                     header-level
                     header)
             (format #t ".LP~%"))
            (else 
             (format #t ".sp 1v~%.LP~%\\fB\\s[+6]~A\\s0\\fP~%.LP~%" header)))
      (when-in-alist (quote "Quote" character)
        (format #t "\\fI“~A”\\fP~%~%" quote))
      (when-in-alist (description "Description" character)
        (format #t "~A~%" description)
        (format #t ".LP~%"))
      (format #t ".KS~%")
      (when-in-alist (scale "Scale" character)
        (format #t "\\fBScale:\\fP ~A~%.br~%" scale))
      (when-in-alist (stats "Statistics" character)
	(set! statistics stats))
      ;; This works for absolute skills listed with stats.
      (if (and (or output-npc-format (assoc "NPC" character))
               (not output-player-format))
          (print-npc-attributes-and-skills character statistics)
          (print-pc-attributes-and-skills character statistics))
      (when-in-alist (perks "Perks" character)
        (when (> (length perks) 0)
	  (format #t "\\fBPerks:\\fP ")
	  (loop for perk in perks
                for i from 1
                when (> i 1) do (format #t ", ")
	        do (match-let (((perk-name perk-dice) perk))
                     ;; we don't care what it cost since we're not
                     ;; calculating costs.
                     (format #t "~A" perk-name)))
          (format #t "~%.br~%")))
      (when-in-alist (complications "Complications" character)
        (when (> (length complications) 0)
          (format #t "\\fBComplications:\\fP ")
          (loop for complication in complications
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" complication))
          (format #t "~%.br~%")))
      ;; Powers and Spells come (almost) right after Perks, because that's
      ;; where Sorcerer will be.
      (when-in-alist (powers "Powers" character)
        (when (> (length powers) 0)
          (format #t "\\fBPowers:\\fP ")
          (loop for power in powers
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" power))
          (format #t "~%.br~%")))
      (when-in-alist (spells "Spells" character)
        (when (> (length spells) 0)
          (format #t "\\fBSpells:\\fP ")
          (loop for spell in spells
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" spell))
          (format #t "~%.br~%")))
      (when-in-alist (gear "Gear" character)
        (when (> (length gear) 0)
          (format #t "\\fBGear:\\fP ")
          (loop for g in gear
                for i from 1
                when (> i 1) do (format #t ", ")
                do (format #t "~A" g))
          (format #t "~%.br~%")))
      (when-in-alist (statics "Static" character)
        (cond ((list? statics)
               (format #t "\\fBStatic:\\fP ")
               (loop for static in statics for i from 1
                     when (> i 1) do (format #t "; ")
                     do (match static
                          ((static-name static-value) 
                           (format #t "~A ~A" static-name static-value))
                          ((? string? static)
                           (format #t "~A" static))))
               (format #t "~%.br~%"))
              (else (format #t "\\fBStatic:\\fP ~A~%.br~%" statics))))
      (when-in-alist (defenses "Defenses" character)
        (cond ((list? defenses)
               (format #t "\\fBDefenses:\\fP ")
               (loop for defense in defenses for i from 1
                     when (> i 1) do (format #t "; ")
                     do (match defense
                          ((defense-name defense-value)
                           (format #t "~A ~A" defense-name defense-value))
                          ((? string? defense)
                           (format #t "~A" defense))))
               (format #t "~%.br~%"))
              (else (format #t "\\fBDefenses:\\fP ~A~%.br~%" defenses))))
      (when-in-alist (move "Move" character)
        (format #t "\\fBMove:\\fP ~A~%.br~%" move))
      (when-in-alist (melee "Melee" character)
        (when (> (length melee) 0)
          (format #t "\\fBMelee:\\fP ")
          (loop for weapon in melee
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%.br~%")))
      (when-in-alist (ranged "Ranged" character)
        (when (> (length ranged) 0)
          (format #t "\\fBRanged:\\fP ")
          (loop for weapon in ranged
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%.br~%")))
      (when-in-alist (natural-armor "Natural Armor" character)
        (when (> (length natural-armor) 0)
          (format #t "\\fBNatural Armor:\\fP ")
          (loop for armor in natural-armor
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" armor))
          (format #t "~%.br~%")))
      (when-in-alist (natural-weapons "Natural Weapons" character)
        (when (> (length natural-weapons) 0)
          (format #t "\\fBNatural Weapons:\\fP ")
          (loop for weapon in natural-weapons
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" weapon))
          (format #t "~%.br~%")))
      (when-in-alist (special-defenses "Special Defenses" character)
        (when (> (length special-defenses) 0)
          (format #t "\\fBSpecial Defenses:\\fP ")
          (loop for defense in special-defenses
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" defense))
          (format #t "~%.br~%")))
      (when-in-alist (special-abilities "Special Abilities" character)
        (when (> (length special-abilities) 0)
          (format #t "\\fBSpecial Abilities:\\fP ")
          (loop for ability in special-abilities
                for i from 1
                when (> i 1) do (format #t "; ")
                do (format #t "~A" ability))
          (format #t "~%.br~%")))
      (when-in-alist (hero-points "Hero_Points" character)
        (format #t "\\fBHero Points:\\fP ~A~%.br~%" hero-points))
      (when-in-alist (fate-points "Fate_Points" character)
        (format #t "\\fBFate Points:\\fP ~A~%.br~%" fate-points))
      (when-in-alist (force-points "Force_Points" character)
        (format #t "\\fBForce Points:\\fP ~A~%.br~%" force-points))
      (cond (output-breachworld
             (format #t "\\fBWL:\\fP D:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □~%")
             (format #t "\\fBSL:\\fP S:1–8 □ SS:9+ □~%"))
            (else
             (format #t "\\fBWL:\\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □~%")))
      ;; Can we implement "Image" usefully?
      ;; This is very output format dependent: .eps for groff -ms and
      ;; pandoc -w ms, .png for pandoc -w html.
      ;; 
      ;; Can we implement output-notes?  We'd have to do something to
      ;; set headings, which would mean moving this back to using headings.
      ;; And then we'd need an option for .NH/.SH.
      (format #t ".KE~%")))

  (define (process-filename filename)
    (let ((ext (pathname-extension filename)))
      (let ((characters
             (cond ((and ext (string=? ext "json"))
                    (with-input-from-file filename read-json))
                   ((and ext (string=? ext "yaml"))
                    (call-with-input-file filename
                      (lambda (port) (yaml-load port))))
                   (else
                    (die 3 "unrecognized extension: \"~A\" in input filename \"~A\"~%"
                         (if ext ext "") filename)))))
        (loop for character in characters
	      for i from 1
	      do (print-character character)))))

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

  (define header-level 1)
  (define output-breachworld #f)
  (define output-generated #f)
  (define output-header #f)
  (define output-notes #t)
  (define output-npc-format #f)
  (define output-number-header #f)
  (define output-player-format #f)
  (define output-player-name #t)
  (define output-title #f)
  (define sort-skills #t)

  (define opts
    (list (args:make-option
           (b breachworld) #:none
           "Output different wound track for Breachworld."
           (set! output-breachworld (not output-breachworld)))
          (args:make-option
           (g generated) #:none
           "Output a generated date only if title specified."
	   (set! output-generated (not output-generated)))
          (args:make-option
           (H header) #:none
           "Toggle output of header (.SH/.NH) (default OFF)"
           (set! output-header (not output-header)))
          (args:make-option
           (h help) #:none "Display this text"
	   (usage))
          (args:make-option
           (l level) #:required
           "Integer indicating level for headers. (default 1)"
           (set! header-level (string->number arg)))
          (args:make-option
           (N number-header) #:none
           (string-append
            "Toggle output of number header (.NH) instead of\n"
            "                          unnumbered (.SH) (default OFF)")
           (set! output-number-header (not output-number-header)))
          (args:make-option
           (n npc-format) #:none "Toggle NPC output format (default OFF)"
	   (set! output-npc-format (not output-npc-format)))
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
           (t title) #:required "Set title to output."
           (set! output-title arg))))
	  

  (receive (options operands)
      (args:parse (command-line-arguments) opts)
    (when #f
      (format #t "output-player-name: ~A~%output-title: ~A~%"
              output-player-name output-title)
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
      (format #t ".TL~%~A~%.MC 3.4i 0.2i~%" output-title)
      (when output-generated
        (format #t ".LP~%\\fIGenerated: ~A\\fP~%"
                (date->string (current-date) "~Y-~m-~d ~T (~A, ~e ~B ~Y)"))))


    (loop for filename in operands
          for i from 1
          when (> i 1) do (format #t "~%~%~%")
          do (process-filename filename)))
  )
;; end of sm6troff.scm
