(in-package #:cl-user)

(defpackage #:aoc-2018-4
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2018-4)

(defun read-events (filename)
  (-> (with-open-file (in filename)
        (loop :for line := (read-line in nil)
              :while line
              :collect (parse-event line)))
      (sort #'local-time:timestamp<
            :key #'event-time)))

(defun parse-event (string)
  (register-groups-bind ((#'parse-spaced-timestring time)
                         (#'parse-integer guard)
                         text)
      ("\\[(.*)\\] (?:Guard #(\\d*))?(.*)" string)
    (apply #'make-event
           :time time
           (cond ((search "begin" text)
                  (list :type :begin
                        :guard guard))
                 ((search "wake" text)
                  (list :type :wake))
                 ((search "sleep" text)
                  (list :type :sleep))
                 (t
                  (error "Event malformed: ~s" string))))))

(defstruct event
  time
  type
  guard)

(defun parse-spaced-timestring (s)
  (local-time:parse-timestring (concatenate 'string s ":00")
                               :date-time-separator #\space))

(defun aoc4a (&optional (events (read-events "4"))
              &aux (guards (make-hash-table)))
  (loop :for event :in events
        :for guard-id := (or (event-guard event) guard-id)
        :when (member (event-type event) '(:sleep :wake))
          :do (push event (gethash guard-id guards nil)))
  (let ((best-guard 
          (loop :for (id . events) :in (hash-table-alist guards)
                :for sleep := (sleep-sum (reverse events))
                :for (best-id best-sleep) := (list id sleep)
                  :then (if (> sleep best-sleep)
                            (list id sleep)
                            (list best-id best-sleep))
                :finally (return best-id))))
    (loop :with minutes := (make-array 60 :initial-element 0)
          :for (sleep wake) :on (reverse (gethash best-guard guards)) :by #'cddr
          :do (loop :for m :from (event-minute sleep) :below (event-minute wake)
                    :do (incf (aref minutes m)))
          :finally (return (values (* (max-index minutes)
                                      best-guard)
                                   (max-index minutes)
                                   best-guard)))))

(defun sleep-sum (events)
  (loop :for (sleep wake) :on events :by #'cddr
        :sum (round (local-time:timestamp-difference (event-time wake)
                                                     (event-time sleep))
                    60)))

(defun event-minute (event)
  (local-time:timestamp-minute (event-time event)))

(defun max-index (vector)
  (loop :for e :across vector
        :for i :upfrom 0
        :for (index value) := (list 0 e) :then (if (> e value)
                                                   (list i e)
                                                   (list index value))
        :finally (return index)))

(defun aoc4b (&optional (events (read-events "4")))
  (let ((guard-sleep-minutes (make-hash-table)))
    (loop :for event :in events
          :for guard-id := (or (event-guard event) guard-id)
          :for sleeping-since := (case (event-type event)
                                   (:sleep (event-minute event))
                                   (:wake sleeping-since)
                                   (t nil))
          :when (eq (event-type event) :wake)
            :do (loop :for minute :from sleeping-since
                        :below (event-minute event)
                      :do
                         (incf
                          (aref (ensure-gethash guard-id
                                                guard-sleep-minutes
                                                (make-array 60
                                                            :initial-element 0))
                                minute))))
    (loop :for (guard-id . minutes) :in (hash-table-alist guard-sleep-minutes)
          :for (best-minute-here best-intensity-here)
            := (loop :for intensity :across minutes
                     :for m :upfrom 0
                     :for (best-minute best-intensity)
                       := (list m intensity)
                         :then (if (> intensity best-intensity)
                                   (list m intensity)
                                   (list best-minute best-intensity))
                     :finally (return (list best-minute
                                            best-intensity)))
          :for (best-guard-id best-minute best-intensity)
            := (list guard-id best-minute-here best-intensity-here)
              :then (if (> best-intensity-here best-intensity)
                        (list guard-id best-minute-here best-intensity-here)
                        (list best-guard-id best-minute best-intensity))
          :finally (return (* best-guard-id best-minute)))))
