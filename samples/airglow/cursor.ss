(import (airglow)
	(airglow-util))

(define cursor 'default)
(define fg 'black)
(define bg 'white)

(define main-window (window 'x 300 'y 300 'w 400 'h 300 'title "Cursors"))
(define choice (menu 'x 80 'y 100 'w 200 'h 25 'title "Cursor:" 'type 'choice))
(define slider1 null)

(define (choice-cb m p)
  (collect-garbage)
  (set! cursor p)
  (valuator-value! slider1 p)
  (airglow-cursor! cursor fg bg))

(menu-add choice "DEFAULT" 'callback choice-cb 
	  'user-data 'default)
(menu-add choice "ARROW" 'callback choice-cb 
	  'user-data 'arrow)
(menu-add choice "CROSS" 'callback choice-cb 
	  'user-data 'cross)
(menu-add choice "WAIT" 'callback choice-cb 
	  'user-data 'wait)
(menu-add choice "INSERT" 'callback choice-cb 
	  'user-data 'insert)
(menu-add choice "HAND" 'callback choice-cb 
	  'user-data 'hand)
(menu-add choice "HELP" 'callback choice-cb 
	  'user-data 'help)
(menu-add choice "MOVE" 'callback choice-cb 
	  'user-data 'move)
(menu-add choice "NS" 'callback choice-cb 
	  'user-data 'ns)
(menu-add choice "WE" 'callback choice-cb 
	  'user-data 'we)
(menu-add choice "NWSE" 'callback choice-cb 
	  'user-data 'nwse)
(menu-add choice "NESW" 'callback choice-cb 
	  'user-data 'nesw)
(menu-add choice "NONE" 'callback choice-cb 
	  'user-data 'none)
(menu-add choice "N" 'callback choice-cb 
	  'user-data 'n)
(menu-add choice "NE" 'callback choice-cb 
	  'user-data 'ne)
(menu-add choice "E" 'callback choice-cb 
	  'user-data 'e)
(menu-add choice "SE" 'callback choice-cb 
	  'user-data 'se)
(menu-add choice "S" 'callback choice-cb 
	  'user-data 's)
(menu-add choice "SW" 'callback choice-cb 
	  'user-data 'sw)
(menu-add choice "W" 'callback choice-cb 
	  'user-data 'w)
(menu-add choice "NW" 'callback choice-cb 
	  'user-data 'nw)

(widget-callback! choice choice-cb)
(widget-callback-when! choice (list 'release 'not-changed))


(define (setcursor slider p)
  (set! cursor (valuator-integer-value slider))
  (airglow-cursor! cursor fg bg))

(set! slider1 (valuator 'x 80 'y 180 'w 310 'h 30 
			'title "Cursor:" 
			'type 'value-slider))
(valuator-slider-type! slider1 'hor)
(widget-label-align! slider1 'left)
(valuator-step! slider1 1)
(valuator-precision slider1 0)
(valuator-bounds slider1 0 100)
(valuator-value! slider1 0)
(widget-callback! slider1 setcursor)
(valuator-value! slider1 (cursor->integer cursor))


(define (setclr slider p)
  (case p
    ((fore) (set! fg (valuator-value slider)))
    ((back) (set! bg (valuator-value slider))))
  (airglow-cursor! cursor fg bg))

(define slider2 (valuator 'x 80 'y 220 'w 310 'h 30 
			  'title "fgcolor:" 
			  'type 'value-slider))
(valuator-slider-type! slider2 'hor)
(widget-label-align! slider2 'left)
(valuator-step! slider2 1)
(valuator-precision slider2 0)
(valuator-bounds slider2 0 255)
(valuator-value! slider2 0)
(widget-callback! slider2 setclr 'fore)

(define slider3 (valuator 'x 80 'y 260 'w 310 'h 30 
			  'title "bgcolor:" 
			  'type 'value-slider))
(valuator-slider-type! slider3 'hor)
(widget-label-align! slider3 'left)
(valuator-step! slider3 1)
(valuator-precision slider3 0)
(valuator-bounds slider3 0 255)
(valuator-value! slider3 0)
(widget-callback! slider3 setclr 'back)

(group-finish main-window)
(window-show main-window)
(airglow-run)
