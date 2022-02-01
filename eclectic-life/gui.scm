;;
;; Conway's Game of Life in Scheme
;; Using the Chickadee graphics and game library
;;
;; This application is written in Guile 3
;;
;; The underlying data structure in the eclectic life module for cells is the Guile
;; array.  Rows in arrays correspond to "rows" in the display system here, and columns
;; in arrays correspond to "columns" here.  This is an arbitrary choice, but one made here.
;;
;; The Chickadee origin is the bottom-left, as opposed to SDL's origin.
;;   (chickadee/chickadee.scm, chickadee/chickadee/graphics/font.scm)
(define-module (eclectic-life gui)
  #:use-module (chickadee)
  #:use-module (chickadee audio)
  #:use-module (chickadee graphics color)
  #:use-module (chickadee graphics path)
  #:use-module (chickadee math easings)
  #:use-module (chickadee math vector)
  #:use-module (chickadee scripting agenda)
  #:use-module (oop goops)
  #:use-module (eclectic-life life)
  #:export (start-eclectic-life load draw update key-press initialize))

(define win-width 640)
(define win-height 480)
;; Universe width and height in terms of number of cells
(define universe-width 50)
(define universe-height 50)
(define cell-width (/ win-width universe-width))
(define cell-height (/ win-height universe-height))
(define canvas (make-empty-canvas))
;; Figure out the bug here, the key-press handler is registering two key presses
(define quitting #f)
(define random-state (random-state-from-platform))

;; The universe, the main game object
(define universe
  (let ((u (make <universe> #:width universe-width #:height universe-height)))
    (begin
      (re-init u #:width universe-width #:height universe-height)
      u)))

(define (my-rectangle position)
  (with-style ((fill-color blue))
    (fill
     ;; parameters: bottom-left corner, width, height
     (rectangle position cell-width cell-height))))

(define (make-main-painter u)
  (if (not (get-cells u))
      (re-init universe #:width universe-width #:height universe-height))
  (let ((cells (get-cells u))
	(items '()))
    (begin
      (do ((row 0 (1+ row)))
	  ((> row (1- universe-height)))
	(do ((column 0 (1+ column)))
	    ((> column (1- universe-width)))
	  ;; Array references are made with the row index first, then the column index
	  (if (array-ref cells row column)
	      ;; For graphics display, the "column" and "row" correspond to x and y
	      (let ((vec (vec2 (* column cell-width) (* row cell-height))))
		(set! items (cons vec items))))))
      (apply superimpose
	     (map
	      (lambda (item) (my-rectangle item))
	      items)))))

;; Update the main painter
(define (move-main-painter universe)
  (begin
    (if (not (get-cells universe))
	(re-init universe #:width universe-width #:height universe-height))
    ;; Update the canvas
    (set-canvas-painter! canvas (make-main-painter (update-universe universe)))))

;; Load the universe and set the painter
(define (load)
  (begin
    (display "loading\n")
    ;; importing and using the initialize method doesn't work
    (if (not (get-cells universe))
	(re-init universe #:width universe-width #:height universe-height))
    (let ((new-universe (make <universe> #:width universe-width #:height universe-height)))
      (begin
	(set! universe new-universe)
	(set-canvas-painter! canvas (make-main-painter universe))))))

;; Update the universe and draw it every two seconds
;; The default update time in the core loop is 60 HZ, sixty times a second
(every 120 (move-main-painter universe))

;; Draw the canvas
(define (draw alpha)
  (draw-canvas canvas))

;; Time units in the agenda are independent of real time.
;; Here we map each each call of the update procedure to one unit of agenda time
;; Then we need to call update periodically
;; The default update time in the core loop is 60 HZ, sixy times a second
(define (update dt)
  (update-agenda 1))

;; Handle key presses
(define (key-press key modifiers repeat?)
  (case key
    ((q)
     (quit))))
