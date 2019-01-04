(in-package #:cl-user)

(defpackage #:aoc-2018-24
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2018-24)

(defun read-groups-from-file ()
  (with-open-file (in "24")
    (read-groups in)))

(defparameter *id-counters* (make-hash-table :test #'equal))

(defun read-groups (stream)
  (clrhash *id-counters*)
  (loop :with side := nil
        :for line := (read-line stream nil)
        :while line
        :when (scan ":" line)
          :do (setf side (subseq line 0 (position #\: line)))
        :when (parse-group line side)
          :collect it))

(defstruct group
  name
  side
  units
  hit-points
  immune
  weak
  attack-damage
  attack-type
  initiative)

(defun parse-group (string side)
  (register-groups-bind ((#'parse-integer units hit-points)
                         (#'parse-attributes attributes)
                         (#'parse-integer attack-damage)
                         (#'keywordize attack-type)
                         (#'parse-integer initiative))
      ("(\\d+) units each with (\\d+) hit points( \\(.*\\))? with an attack that does (\\d+) (.*) damage at initiative (\\d+)"
       string)
    (make-group :name (strcat side " group " (incf (gethash side *id-counters* 0)))
                :side side
                :units units
                :hit-points hit-points
                :immune (cdr (assoc :immune attributes))
                :weak (cdr (assoc :weak attributes))
                :attack-damage attack-damage
                :attack-type attack-type
                :initiative initiative)))

(defun parse-attributes (string)
  (mapcar (lambda (part)
            (->> (string-trim "() " part)
                 (split "( to |, )")
                 (mapcar #'keywordize)))
          (split "; " string)))

(defun keywordize (string)
  (intern (string-upcase string) "KEYWORD"))

(defun effective-power (group)
  (* (group-units group)
     (group-attack-damage group)))

(defun damage (attacker defender)
  (* (effective-power attacker)
     (cond ((member (group-attack-type attacker)
                    (group-immune defender))
            0)
           ((member (group-attack-type attacker)
                    (group-weak defender))
            2)
           (t
            1))))

(defun fight (groups)
  (let ((attacks
          (loop :for attacker :in (sort-by (copy-list groups)
                                           (#'> #'effective-power #'=)
                                           (#'> #'group-initiative))
                :for targets := (copy-list groups) :then remaining
                :for defender := (select-target attacker targets)
                :for remaining := (delete defender targets)
                :when defender
                  :collect (list attacker defender))))
    #+debug
    (terpri)
    (loop :for (attacker defender)
            :in (sort attacks #'> :key (compose #'group-initiative #'first))
          :for damage := (damage attacker defender)
          :for losses := (floor damage (group-hit-points defender))
          :when (plusp (group-units attacker))
            :do #+debug
                (format t "~a attacks ~a, killing ~a units~%"
                        (group-name attacker)
                        (group-name defender)
                        (min losses (group-units defender)))
                (decf (group-units defender) losses))
    #+debug
    (terpri)
    groups))

(defun select-target (attacker targets)
  (let ((valid-targets (remove-if (lambda (target)
                               (or (string= (group-side attacker)
                                            (group-side target))
                                   (zerop (damage attacker target))))
                                  targets)))
    #+debug
    (when (= (length valid-targets) 1)
      (format t "~a would deal ~a ~a damage~%"
              (group-name attacker)
              (group-name (first valid-targets))
              (damage attacker (first valid-targets))))
    (first (sort-by valid-targets
                    (#'>
                     (lambda (target)
                       (let ((damage (damage attacker target)))
                         #+debug
                         (format t "~a would deal ~a ~a damage~%"
                                 (group-name attacker)
                                 (group-name target)
                                 damage)
                         damage))
                     #'=)
                    (#'> #'effective-power #'=)
                    (#'> #'group-initiative)))))

(defun combat (groups)
  #+debug
  (print-stats groups)
  (loop :for last-sum := nil :then this-sum
        :for carnage := (fight groups) :then (fight remaining)
        :for remaining := (remove-if (lambda (group)
                                       (not (plusp (group-units group))))
                                     carnage)
        :for this-sum := (count-units remaining)
        :for sides := (remove-duplicates remaining
                                         :test #'equal
                                         :key #'group-side)
        #+debug #+debug
        :do (print-stats remaining)
        :when (and last-sum (= this-sum last-sum))
          :return (values remaining
                          "Draw")
        :when (= (length sides) 1)
          :return (values remaining
                          (group-side (first remaining)))))

(defun print-stats (groups)
  (format t "~{~{~a contains ~a units~%~}~}~%"
          (mapcar (lambda (group)
                    (list (group-name group) (group-units group)))
                  (sort (copy-list groups)
                        #'string<
                        :key #'group-name))))

(defun aoc24a (&optional (groups (read-groups-from-file)))
  (let ((survivors (combat groups)))
    (count-units survivors)))

(defun count-units (groups)
  (reduce #'+ (mapcar #'group-units groups)))

(defun aoc24b (&optional (groups (read-groups-from-file)))
  (loop :for boost := 1 :then next-boost
        :for (survivors winner)
          := (multiple-value-list (boosted-combat boost groups))
        :for winp := (string= winner "Immune System")
        :for smallest-win := (if winp boost nil)
          :then (if winp boost smallest-win)
        :for largest-fail := (if winp 0 boost)
          :then (if winp largest-fail boost)
        :for next-boost := (if smallest-win
                               (ceiling (+ smallest-win largest-fail)
                                        2)
                               (* 2 largest-fail))
        :do (print (list :smallest-win smallest-win
                         :largest-fail largest-fail
                         :next-boost next-boost))
        :when (and smallest-win
                   (= (- smallest-win largest-fail) 1))
          :return (values (count-units (boosted-combat smallest-win groups))
                          smallest-win)))

(defun boosted-combat (boost groups)
  (let ((boosted-groups
          (mapcar (lambda (group)
                    (let ((copy (copy-group group)))
                      (when (string= (group-side copy) "Immune System")
                        (incf (group-attack-damage copy) boost))
                      copy))
                  groups)))
    (combat boosted-groups)))
