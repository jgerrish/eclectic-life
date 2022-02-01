;;
;; The main application for eclectic-life
;;
;; Run with: GUILE_LOAD_PATH=$GUILE_LOAD_PATH:. chickadee play eclectic-life.scm
;;
(use-modules (eclectic-life gui))

(define win-width 640)
(define win-height 480)

(run-game #:window-title "Conway's Game of Life"
	  #:window-width win-width
	  #:window-height win-height
          #:load load
          #:draw draw
          #:update update
	  #:key-press key-press)
