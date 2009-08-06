(import (airglow))

(define N 0)
(define W 200)
(define H 50)
(define ROWS 14)

(define (bt name type)
  (let ((x (modulo N 4))
	(y (/ N 4))
	(b null)
	(w 0) (h 0))
    (set! N (+ N 1))
    (set! x (+ (* x W) 10))
    (set! y (+ (* y H) 10))
    (set! w (- W 20))
    (set! h (- H 20))
    (if (not (integer? x))
	(set! x (floor x)))
    (if (not (integer? y))
	(set! y (floor y)))
    (set! b (border 'x x 'y y 'w w 'h h 'title name 'type type))
    (widget-label-size! b 11)))

(define main-window (window 'w (* 4 W)
			    'h (* ROWS H)
			    'title "Boxtypes"
			    'type 'double))

(widget-border! main-window 'flat)
(airglow-system-colors)
(widget-bg-color! main-window 12)

(bt "NONE" 'none)
(bt "FLAT" 'flat)
(set! N (+ N 2))
(bt "UP" 'up)
(bt "DOWN" 'down)
(bt "UP-FRAME" 'up-frame)
(bt "DOWN-FRAME" 'down-frame)
(bt "THIN-UP" 'thin-up)
(bt "THIN-DOWN" 'thin-down)
(bt "THIN-UP-FRAME" 'thin-up-frame)
(bt "THIN-DOWN-FRAME" 'thin-down-frame)
(bt "ENGRAVED" 'engraved)
(bt "EMBOSSED" 'embossed)
(bt "ENGRAVED_FRAME" 'engraved-frame)
(bt "EMBOSSED_FRAME" 'embossed-frame)
(bt "BORDER" 'border)
(bt "SHADOW" 'shadow)
(bt "BORDER-FRAME" 'border-frame)
(bt "SHADOW-FRAME" 'shadow-frame)
(bt "ROUNDED" 'rounded)
(bt "RSHADOW" 'rshadow)
(bt "ROUNDED_FRAME" 'rounded)
(bt "RFLAT" 'rflat)
(bt "OVAL" 'oval)
(bt "OSHADOW" 'oshadow)
(bt "OVAL-FRAME" 'oval-frame)
(bt "OFLAT" 'oflat)
(bt "ROUND-UP" 'round-up)
(bt "ROUND-DOWN" 'round-down)
(bt "DIAMOND-UP" 'diamond-up)
(bt "DIAMOND-DOWN" 'diamond-down)
(bt "PLASTIC-UP" 'plastic-up)
(bt "PLASTIC-DOWN" 'plastic-down)
(bt "PLASTIC-UP-FRAME" 'plastic-up)
(bt "PLASTIC-DOWN-FRAME" 'plastic-down)
(bt "PLASTIC-THIN-UP" 'plastic-thin-up)
(bt "PLASTIC-THIN-DOWN" 'plastic-thin-down)
(set! N (+ N 2))
(bt "PLASTIC-ROUND-UP" 'plastic-round-up)
(bt "PLASTIC-ROUND-DOWN" 'plastic-round-down)
(set! N (+ N 2))
(bt "GTK-UP" 'gtk-up)
(bt "GTK-DOWN" 'gtk-down)
(bt "GTK-UP-FRAME" 'gtk-up-frame)
(bt "GTK-DOWN-FRAME" 'gtk-down-frame)
(bt "GTK-THIN-UP" 'gtk-thin-up)
(bt "GTK-THIN-DOWN" 'gtk-thin-down)
(bt "GTK-THIN-UP-FRAME" 'gtk-thin-up-frame)
(bt "GTK-THIN-DOWN-FRAME" 'gtk-thin-down-frame)
(bt "GTK-ROUND-UP" 'gtk-round-up)
(bt "GTK-ROUND-DOWN" 'gtk-round-down)

(group-resizable main-window main-window)
(group-finish main-window)
(window-show main-window)
(airglow-run)