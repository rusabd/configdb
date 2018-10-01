(in-package :configdb)

;; ranges are sets of intervals.

(defclass range ()
  ((first :initform 0 :initarg :first :reader range-first)
   (last :initform 0 :initarg :last  :reader range-last)))

(defmethod print-object ((range range) stream)
  (print-unreadable-object (range stream :type t :identity t)
    (format stream "[~A:~A]" (range-first range) (range-last range))))

(defclass window ()
  ((ranges :initform nil :initarg :ranges :reader window-ranges)))

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (format stream "<<~{~A~^,~}>>" (slot-value window 'ranges))))

(defun test-range ()
  (let ((r1 (random 100))
        (r2 (random 100)))
    (make-instance 'range :first (min r1 r2) :last (max r1 r2))))

(defun test-window ()
  (loop with items = (1+ (random 9))
       repeat items
     collect (test-range) into ranges
     finally (return (make-instance 'window :ranges ranges))))


;; 12679109


;; ...+----+..
;; ....+------+...
;; .....+--+........
;; ..............+----+...
;; -->
;; ...+-------+..+----+....

(defun merge-window (window)
  (loop
     with sorted-ranges = (sort (window-ranges window) #'< :key 'range-first)
     with min-val = 0
     with max-val = 0
     for range in sorted-ranges
     if (> max-val (range-first range)) 
     ;; include current interval
     ;; min-val stay the same
       do (setf max-val (max max-val (range-last range)))
     else
     ;; generate a new interval
     collect (list min-val max-val) into intervals
       and do (setf min-val (range-first range))
       and do (setf max-val (range-last range))
     end
     finally 
       (return (make-window (reverse (cons (list min-val max-val)
					   (reverse (rest intervals))))))))


(defun make-window (ranges-list)
  (make-instance 'window :ranges
		 (loop for (low high) in ranges-list
		    collect (make-instance 'range :first low :last high))))
