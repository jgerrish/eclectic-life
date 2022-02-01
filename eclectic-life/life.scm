;; The rules for Conway's Game of Life
;; These are the standard rules from the Game of Life (B3/S23)
;;
;; The universe is a rectangular / square grid
;; Every cell has eight neighbors (diagonals are neighbors)
;;
;; 1. Any live cell with fewer than two neighbors dies (underpopulation)
;; 2. Any live cell with two or three neighbors lives (life)
;; 3. Any live cell with more than three neighbors dies (overpopulation)
;; 4. Any dead cell with exactly three live neighbors becomes a live cell (reproduction)
;;
;; TODO: second version, more intelligent data structure for neighbor state
;; TODO: There's a lot of comments around consistency between array addressing
;;       and display coordinates (rows, columns, x, y, etc.)
;;       Add some data structures and constraints to make mistakes less likely
;;
(define-module (eclectic-life life)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:export (<universe> get-cells set-cells! get-width get-height initialize re-init
		       get-wrapped-coordinate cell-neighbors live-or-die get-cell-states
		       count-live-cells new-cell-state next-generation update-universe))

;; Generate a random true or false value
(define (random-boolean)
  ((lambda (x) (if (= 0 x) #f #t)) (random 2)))

;; Generate the initial board
(define (generate-cells width height)
  ;; make-array initial-value num-rows num-columns
  (let ((cells (make-array #f height width)))
    (begin
      (array-map! cells random-boolean);; (lambda () (random-boolean seed)))
      cells)))

;; The universe is a class containing the set of game of life cells
(define-class <universe> ()
  (width #:init-value 10 #:getter get-width #:setter set-width! #:accessor width
	 #:init-keyword #:width)
  (height #:init-value 10 #:getter get-height #:setter set-height! #:accessor height
	  #:init-keyword #:height)
  ;; The cells in the universe
  ;; This is a guile array
  ;; Guile arrays can be

  ;; For example, consider the array below (created using the array tag format):
  ;; #2((a b c) (d e f) (g h i))
  ;; (a b c) is considered a row (Guile info page 6.6.13.3 Shared Arrays)
  ;; To access an element:
  ;; (array-ref #2((a b c) (d e f) (g h i)) 0 2) would return c
  ;; So the first index parameter is the row index
  ;; The second index parameter is the column index
  (cells #:init-form #f
	 #:getter get-cells #:setter set-cells! #:accessor cells))

(define-method (initialize (universe <universe>) . initargs)
  (begin
    (display "initializing universe\n")
    (next-method)
    (slot-set! universe 'cells
	       (generate-cells (get-width universe) (get-height universe)))))

(define-method (re-init (universe <universe>) . args)
  (let-keywords args #f ((width 10)
			 (height 10))
		(begin
		  (display "re-initializing universe\n")
		  (set-width! universe width)
		  (set-height! universe height)
		  (slot-set! universe 'cells
			     (generate-cells (get-width universe) (get-height universe))))))

;; Wrap on boundaries
(define-method (get-wrapped-coordinate (universe <universe>) coordinate)
  ;; The first coordinate is the row index, corresponding to the height
  ;; The second coordinate is the column index, corresponding to the width
  (let ((x (modulo (first coordinate) (get-height universe)))
	(y (modulo (second coordinate) (get-width universe))))
    (list x y)))

;; Distance to each neighbor
;; The default format in data formats and API is to have width first, followed by height
;; Any transformation to array coordinates is done automatically
(define neighbor-distances
  (list '(-1 -1) '(-1 0) '(-1 1) '(0 -1) '(0 1) '(1 -1) '(1 0) '(1 1)))

;; Return a list of all neighbors of this cell, as lists of x, y coordinates
;; TODO: Standardize on cell coordinate addresses (single list, two variables, etc.)
(define-method (cell-neighbors (universe <universe>) cell-row-index cell-column-index)
  ;; Wrap the neighbor address if we need to
  (map
   (lambda (x) (get-wrapped-coordinate universe x))
   ;; Get the neighbors of the specific cell passed in
   (map
    (lambda (neighbor) (list (+ cell-row-index (first neighbor))
			     (+ cell-column-index (second neighbor))))
    neighbor-distances)))

;; Return #t if the cell should live, #f if the cell should die
(define (live-or-die my-state number-live-neighbors)
  (if my-state
      ;; live cell
      (cond
       ((< number-live-neighbors 2)
	#f)
       ((or (= number-live-neighbors 2) (= number-live-neighbors 3))
	#t)
       ((> number-live-neighbors 3)
	#f))
      ;; dead cell
      (if (= number-live-neighbors 3)
	  #t
	  #f)))

;; map from cell addresses to cell states
;; Returns a list of boolean values
(define-method (get-cell-states (universe <universe>) list)
  (let ((cells (get-cells universe)))
    (map
     (lambda (coord)
       (array-ref cells (first coord) (second coord)))
     list)))

;; Count the number of true values in a list
(define-method (count-live-cells (universe <universe>) list)
  (fold (lambda (elem prev) (+ (if elem 1 0) prev)) 0 list))

;; Get the new cell state for a cell
(define-method (new-cell-state (universe <universe>) cell-row-index cell-column-index)
  (let* ((cells (get-cells universe))
	 (neighbors (cell-neighbors universe cell-row-index cell-column-index))
	 (my-state (array-ref cells cell-row-index cell-column-index))
	 (states (get-cell-states universe neighbors))
	 (number-live-neighbors
	  (count-live-cells universe states)))
    (live-or-die my-state number-live-neighbors)))

;; Build the next generation
;; Returns the new cell array, it's up to the caller to update the universe object
(define-method (next-generation (universe <universe>))
  (let ((new-array (make-array #f (get-height universe) (get-width universe))))
    (begin
      (array-index-map!
       new-array
       (lambda (row column) (new-cell-state universe row column)))
      new-array)))

;; The main update procedure
;; Generates a new universe from the last universe
;; Returns the new universe
(define-method (update-universe (universe <universe>))
  (begin
    (set-cells! universe (next-generation universe))
    universe))
