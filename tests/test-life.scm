;;
;; Test the methods and functions in the life module
;;
(define-module (tests life)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (eclectic-life life))

;; Test wrapping at the edge of the universe
(test-begin "get-wrapped-coordinate")
(let ((universe (make <universe> #:width 2 #:height 2)))
  (begin
    (test-equal "(0 0) -> (0 0)" '(0 0) (get-wrapped-coordinate universe '(0 0)))
    (test-equal "(0 1) -> (0 1)" '(0 1) (get-wrapped-coordinate universe '(0 1)))
    (test-equal "(1 0) -> (1 0)" '(1 0) (get-wrapped-coordinate universe '(1 0)))
    (test-equal "(1 1) -> (1 1)" '(1 1) (get-wrapped-coordinate universe '(1 1)))

    (test-equal "(0 2) -> (0 0)" '(0 0) (get-wrapped-coordinate universe '(0 2)))
    (test-equal "(0 3) -> (0 1)" '(0 1) (get-wrapped-coordinate universe '(0 3)))
    (test-equal "(2 0) -> (0 0)" '(0 0) (get-wrapped-coordinate universe '(2 0)))
    (test-equal "(3 0) -> (1 0)" '(1 0) (get-wrapped-coordinate universe '(3 0)))))
(test-end)

;; Test listing cell neighbors
(test-begin "cell-neighbors")
(let ((universe (make <universe> #:width 2 #:height 2)))
  (begin
    (test-equal "(0 0) -> (0 0)"
      (list '(1 1) '(1 0) '(1 1) '(0 1) '(0 1) '(1 1) '(1 0) '(1 1))
      (cell-neighbors universe 0 0))
    (test-equal "(0 1) -> (0 1)"
      (list '(1 0) '(1 1) '(1 0) '(0 0) '(0 0) '(1 0) '(1 1) '(1 0))
      (cell-neighbors universe 0 1))
    (test-equal "(1 0) -> (1 0)"
      (list '(0 1) '(0 0) '(0 1) '(1 1) '(1 1) '(0 1) '(0 0) '(0 1))
      (cell-neighbors universe 1 0))
    (test-equal "(1 1) -> (1 1)"
      (list '(0 0) '(0 1) '(0 0) '(1 0) '(1 0) '(0 0) '(0 1) '(0 0))
      (cell-neighbors universe 1 1))))
(test-end)

;; Test the main rule engine for whether a cell should live or die
(test-begin "live-or-die")
;; Test the cases where the current cell is dead
(let ((my-state #f))
  (begin
    (test-equal "dead, 0 neighbors" #f (live-or-die my-state 0))
    (test-equal "dead, 1 neighbors" #f (live-or-die my-state 1))
    (test-equal "dead, 2 neighbors" #f (live-or-die my-state 2))
    (test-equal "dead, 3 neighbors" #t (live-or-die my-state 3))
    (test-equal "dead, 4 neighbors" #f (live-or-die my-state 4))
    (test-equal "dead, 5 neighbors" #f (live-or-die my-state 5))
    (test-equal "dead, 6 neighbors" #f (live-or-die my-state 6))
    (test-equal "dead, 7 neighbors" #f (live-or-die my-state 7))
    (test-equal "dead, 8 neighbors" #f (live-or-die my-state 8))))
;; Test the cases where the current cell is alive
(let ((my-state #t))
  (begin
    (test-equal "alive, 0 neighbors" #f (live-or-die my-state 0))
    (test-equal "alive, 1 neighbors" #f (live-or-die my-state 1))
    (test-equal "alive, 2 neighbors" #t (live-or-die my-state 2))
    (test-equal "alive, 3 neighbors" #t (live-or-die my-state 3))
    (test-equal "alive, 4 neighbors" #f (live-or-die my-state 4))
    (test-equal "alive, 5 neighbors" #f (live-or-die my-state 5))
    (test-equal "alive, 6 neighbors" #f (live-or-die my-state 6))
    (test-equal "alive, 7 neighbors" #f (live-or-die my-state 7))
    (test-equal "alive, 8 neighbors" #f (live-or-die my-state 8))))
(test-end)


;; Test getting the states of cells from a list of cell addresses
(test-begin "get-cell-states")
(let ((universe (make <universe> #:width 2 #:height 2)))
  (begin
    ;; Set up a test universe with known values
    (set-cells! universe #2((#f #t) (#f #f)))
    (test-equal
	"get-cell-states"
      (list #f #t #f #f)
      (get-cell-states universe (list '(0 0) '(0 1) '(1 0) '(1 1))))))
(test-end)

;; Test counting live cells in a list of states
(test-begin "count-live-cells")
(let ((universe (make <universe> #:width 2 #:height 2)))
  (begin
    ;; Set up a test universe with known values
    (set-cells! universe #2((#f #f) (#f #t)))
    (test-equal "count-live-cells 0" 0 (count-live-cells universe (list #f #f #f #f)))
    (test-equal "count-live-cells 4" 4 (count-live-cells universe (list #t #t #t #t)))
    (test-equal "count-live-cells 1 end" 1 (count-live-cells universe (list #f #f #f #t)))
    (test-equal "count-live-cells 1 beginning" 1 (count-live-cells universe (list #t #f #f #f)))
    (test-equal "count-live-cells 2" 2 (count-live-cells universe (list #t #t #f #f)))))
(test-end)

(test-begin "new-cell-state")
(let ((universe (make <universe> #:width 2 #:height 2)))
  (begin
    ;; Set up a test universe with known values
    (set-cells! universe #2((#f #t) (#f #t)))
    ;; (display (format #f "universe: ~a\n" (get-cells universe)))
    (test-equal "0 0 -> #f" #f (new-cell-state universe 0 0))
    (test-equal "0 1 -> #f" #t (new-cell-state universe 0 1))
    (test-equal "1 0 -> #f" #f (new-cell-state universe 1 0))
    (test-equal "1 1 -> #f" #t (new-cell-state universe 1 1))
    ))
(test-end)

(test-begin "next-generation")
(begin
  ;; Test a universe that doesn't change
  (let ((universe (make <universe> #:width 3 #:height 3)))
    (begin
      (set-cells! universe #2((#f #f #t) (#t #t #f) (#t #f #f)))
      (let ((new-cells (next-generation universe)))
	(begin
	  (test-equal "same cells"
	    #2((#f #f #t) (#t #t #f) (#t #f #f))
	    new-cells)))))
  ;; Test a universe that changes the next generation
  (let ((universe (make <universe> #:width 2 #:height 2)))
    (begin
      (set-cells! universe #2((#t #f) (#f #f)))
      (let ((new-cells (next-generation universe)))
	(begin
	  (test-equal "different cells"
	    #2((#f #f) (#f #f))
	    new-cells))))))
(test-end)

(test-begin "update-universe")
(begin
  (let ((universe (make <universe> #:width 3 #:height 3)))
    ;; Test a universe that doesn't change
    (begin
      (set-cells! universe #2((#f #f #t) (#t #t #f) (#t #f #f)))
      (let ((new-universe (update-universe universe)))
	(begin
	  (test-equal "same universe"
	    #2((#f #f #t) (#t #t #f) (#t #f #f))
	    (get-cells new-universe))))))
  ;; Test a universe that changes the next generation
  (let ((universe (make <universe> #:width 2 #:height 2)))
    (begin
      (set-cells! universe #2((#t #f) (#f #f)))
      (let ((new-universe (update-universe universe)))
	(begin
	  (test-equal "different universe"
	    #2((#f #f) (#f #f))
	    (get-cells new-universe)))))))
(test-end)
