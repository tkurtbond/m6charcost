;;; sm6fmt.scm - calculate points costs of Mini-Six Characters in JSON.
;;;              uses fmt for output.
;;;
;;; Remember: ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-2.json
(module sm6fmt ()
  (import fmt)
  (import loop)
  (import scheme)
  (import matchable)
  (import medea)
  (import simple-loops)
  (import (srfi 13))
  (import (srfi 69))
  (import (chicken base))
  (import (chicken irregex))
  (import (chicken port))
  (import (chicken process-context))

  (define (die status msg)
    (fmt (current-error-port) (program-name) ": fatal error: " msg nl)
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

  (define (dice-to-cost dice)
    (let ((m (irregex-match *scanner* dice)))
      (unless m                         ; can't decide which is better.
        (if #f (die 1 (fmt #f "invalid dice: " dice))
            (error 'dice-to-cost (fmt #f "invalid dice: " dice))))
      (let* ((num-dice (string->number (irregex-match-substring m 1)))
             (num-pips (irregex-match-substring m 3))
             (num-pips (if num-pips (string->number num-pips) 0)))
        (+ (* num-dice *pips-per-die*)
           (if num-pips num-pips 0)))))

  (define (cost-to-dice cost)
    (let ((dice (truncate (/ cost *pips-per-die*)))
          (pips (modulo cost *pips-per-die*)))
      (fmt #f (num dice) "D+" (num pips))))

  (define (calculate-character character)
    (let ((statistics '("Might" "Agility" "Wit" "Charm"))
          (total-stat-cost 0)
          (total-skill-cost 0)
          (total-perk-cost 0)
          (total-skill-and-perk-cost 0))
      (when-in-hash (name "Name" character)
        (fmt #t "Name: " name nl))
      (when-in-hash (desc "Description" character)
        (fmt #t "    " desc nl))
      (when-in-hash (player "Player" character)
        (fmt #t "Player: " player nl))
      (when-in-hash (stats "Statistics" character)
        (set! statistics (vector->list stats)))
      ;; TODO: Check what form skills are listed, relative or absolute.
      ;; TODO: Check where skills listed, with stats or under 'skills'
      ;; 
      ;; This works for absolute skills listed with stats.
      (do-list stat-name statistics
        (unless (hash-table-exists? character stat-name)
          (die 1 (fmt #f "Missing stat name: " stat-name)))
        (let ((stat-value (hash-table-ref character stat-name)))
          (match-let ((#(stat-dice skills ...) stat-value))
          (let ((stat-cost (dice-to-cost stat-dice)))
            (inc! total-stat-cost stat-cost)
            (fmt #t (pad 30 stat-name ": " stat-dice) " ("
                 (pad/left 3 (num stat-cost)) " points)" nl)
            (loop for skill in skills
                  do (begin
                       (match-let ((#(skill-name skill-dice) skill))
                         (let* ((skill-cost (dice-to-cost skill-dice))
                                (relative-cost (- skill-cost stat-cost))
                                (relative-dice (cost-to-dice relative-cost)))
                           (inc! total-skill-cost relative-cost)
                           (inc! total-skill-and-perk-cost relative-cost)
                           (fmt #t  "    " (pad 19 skill-name ": " skill-dice)
                                " " (pad/left 6 "+" relative-dice) " ("
                                (pad/left 3 (num relative-cost)) " points)" nl)))))))))
      (when-in-hash (perks "Perks" character)
        (fmt #t "Perks:" nl)
        (loop for perk across perks
              do (match-let ((#(perk-name perk-dice) perk))
                   (let ((perk-cost (dice-to-cost perk-dice)))
                     (inc! total-perk-cost perk-cost)
                     (inc! total-skill-and-perk-cost perk-cost)
                     (fmt #t "    " (pad 26 perk-name ": " perk-dice) " ("
                          (pad/left 3 (num perk-cost)) " points)" nl)))))
      (let ((total-stat-dice (cost-to-dice total-stat-cost))
            (total-skill-dice (cost-to-dice total-skill-cost))
            (total-perk-dice (cost-to-dice total-perk-cost))
            (total-skill-and-perk-dice (cost-to-dice total-skill-and-perk-cost)))
        (fmt #t (pad 24 "total stat: ") (pad/left 6 total-stat-dice)
             " (" (pad/left 3 total-stat-cost) " points)" nl)
        (fmt #t (pad 24 "total skill:") (pad/left 6 total-skill-dice)
             " (" (pad/left 3 total-skill-cost) " points)" nl)
        (fmt #t (pad 24 "total perk:") (pad/left 6 total-perk-dice)
             " (" (pad/left 3 total-perk-cost) " points)" nl)
        (fmt #t (pad 24 "total (skill + perk):") (pad/left 6 total-skill-and-perk-dice)
             " (" (pad/left 3 total-skill-and-perk-cost) " points)" nl)
        (let* ((total-cost (+ total-stat-cost total-skill-cost
                              total-perk-cost))
               (total-dice (cost-to-dice total-cost)))
          (fmt #t (pad 24 "total:") (pad/left 6 total-dice)
               " (" (pad/left 3 total-cost) " points)" nl)))))

  (define (process-filename filename)
    (let ((characters (with-input-from-file filename read-json)))
      (loop for character across characters
            for i from 1
            when (> i 1) do (fmt #t *line-of-equals* nl)
            do (calculate-character character))))

  ;; We want 
  (json-parsers
   `(;; Don't change key to symbol
     (member . ,(lambda (name value) (cons name value)))
     ;; Convert objects from alist to hash table
     (object . ,(lambda (object) (alist->hash-table object)))
     ,@(json-parsers)))

  (match (command-line-arguments)
    (()
     (die 1 "No file specified"))
    ((filename) (process-filename filename))
    ((filename . others) 
     (die 1 (fmt #f "unexpected command line arguments: " (wrt others)))))

  )
;; end of m6.scm
