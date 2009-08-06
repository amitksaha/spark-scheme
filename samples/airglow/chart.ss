(import (airglow))

(define window (window 'x 10 'y 20 
		       'w 800 'h 600 
		       'title "Charts Sample"))

(define (add-chart-values c)
  (chart-add c 0.02 "red" 'red)
  (chart-add c 0.03 "blue" 'blue)
  (chart-add c 0.02 "green" 'green)
  (chart-add c 0.04 "black" 'black))

(define my-chart (chart 'x 10 'y 10 'w 250 'h 200 'title "Bar Chart"))
(add-chart-values my-chart)

(set! my-chart (chart 'x 265 'y 10 'w 250 'h 200 'title "Pie Chart" 'type 'special-pie))
(add-chart-values my-chart)

(set! my-chart (chart 'x (+ 270 250) 'y 10 'w 250 'h 200 'title "Line Chart" 'type 'line))
(add-chart-values my-chart)

(define y (+ 10 260))

(set! my-chart (chart 'x 10 'y y 'w 250 'h 200 'title "Horbar Chart" 'type 'horbar))
(add-chart-values my-chart)

(set! my-chart (chart 'x 265 'y y 'w 250 'h 200 'title "Spike Chart" 'type 'spike))
(add-chart-values my-chart)

(set! my-chart (chart 'x (+ 270 250) 'y y 'w 250 'h 200 'title "Filled Chart" 'type 'filled))
(add-chart-values my-chart)

(group-finish window)
(window-show window)
(airglow-run)

