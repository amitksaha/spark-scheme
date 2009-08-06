(import (util) (net) (reactor) (airglow))

(define user-socket null)
(define running #f)
(define chat-client null)
(define args (argv))
(define host "127.0.0.1")
(define port 7070)
(define user "Anonymous")

(define (print-usage)
  (printf "Usage: spark chat-client.ss [options] user host port~n")
  (printf "options:~n")
  (printf "-h  --help        Print this usage instruction and exit.~n~n"))

(define no-args (or (eqv? args null)
		    (not (= (vector-length args) 3))))

(if no-args
    (begin
      (print-usage)
      (exit)))

(set! user (vector-ref args 0))
(set! host (vector-ref args 1))
(set! port (string->number (vector-ref args 2)))

; (define client-socket null)

(define (connect)
;   (printf "~a:~a~n" host port)
;   (set! client-socket (socket))
;   (socket-open client-socket)
;   (socket-connect client-socket (address host port))
;   (socket-send-line client-socket user)
;   (set! running #t))

  (set! chat-client (socket-connector (list 
				       (address host port))))
  (connector-on-connect! chat-client on-connect)
  (connector-on-write! chat-client on-write)
  (connector-open chat-client (list 10 0))
  (set! running #t)
  (airglow-thread-manager-start)
  (thread check-connector))

(define (disconnect)
;   (socket-send-line client-socket "BYE")
;   (socket-close client-socket)
;   (set! running #f))

  (connector-remove-watch chat-client 
			  user-socket 
			  'for-write)
  (socket-close user-socket)
  (set! running #f)
  (connector-close chat-client)
  (set! chat-client null))  
	
(define (connect-cb b a)
  (if (not running)
      (begin
	(connect)
	(widget-label! b "&Disconnect"))
      (begin
	(disconnect)
	(widget-label! b "&Connect"))))
	
(define (quit-cb b a)
  (if running
      (disconnect))
  (exit))

(define (input-message-cb i a)
  (if running
      (begin
	(let ((msg (input-field-value i)))
	  (socket-non-blocking! user-socket #f)
	  (socket-send-line user-socket msg)
	  (browser-add messages msg)))))

(define window (window 'w 740 'h 400))
(define input-message (input-field 'x 70 'y 10 'w 550 'h 30 'title "Message:"))
(widget-callback! input-message input-message-cb 'enter-key)
(widget-tooltip! input-message "Type your message and press Enter.")
(define send-button (button 'x (+ 70 550 10) 'y 10 'w 100 'h 30 'title "&Send"))
(widget-callback! send-button input-message-cb)
(define messages (browser 'x 70 'y 50 'w 550 'h 250 'type 'select))
(define connect-button (button 'x 70 'y 310 'w 100 'h 30 'title "&Connect"))
(widget-callback! connect-button connect-cb)
(define quit-button (button 'x 180 'y 310 'w 100 'h 30  'title "&Quit"))
(widget-callback! quit-button quit-cb)

(group-finish window)
(window-show window)

(define (on-connect connector client-socket)
  (socket-send-line client-socket user)
  (set! user-socket client-socket))

(define (on-write connector client-socket)
  (socket-non-blocking! client-socket #t)
  (let ((msg (socket-recv-line client-socket)))
    (if (not (eqv? msg null))
	(browser-add messages msg)))
  (socket-non-blocking! client-socket #f))

(define (check-connector)
  (let loop ()
    (if running
	(begin
	  (connector-watch chat-client)
	  (sleep 1)
	  (loop))))
  (airglow-thread-manager-stop))

(airglow-run)
