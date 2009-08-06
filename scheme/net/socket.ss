;; A wrapper over low-level socket calls.
;; Copyright (C) 2008  Vijay Mathew Pandyalakal
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
  
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
  
;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.
  
;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(library net-socket

	(import (exception)
		(asserts)
		(net-address)
		((prefix spark.socket:: #%spark-socket)))

	(export socket socket-instance socket? 
		socket-open socket-bind socket-connect
		socket-listen socket-accept socket-recv
		socket-recv-line socket-recv-bytes socket-recv-from
		socket-recv-bytes-from socket-send socket-send-line
		socket-send-bytes socket-send-bytes-to socket-option! 
		socket-option socket-close socket-non-blocking!
		socket-async-io! socket-non-blocking? socket-async-io?
		socket-handle socket-handle!)

	;; Represents the memory layout of a network socket.
	(define-struct socket-s (handle))

	(define sockets (make-hash-table))
	
	;; Creates and initializes a socket object.
	;; Returns the new socket object on success.
	(define (socket . args)
	  (let ((self (make-socket-s null)))
	    (if (not (eqv? args null))
		(socket-handle! self (car args)))
	    self))

	(define (socket? self)
	  (socket-s? self))

	;; Initializes the low-level socket handle.
	;; Raises an exception if the call fails.
	;; Returns #t on success.
	(define socket-open
	  (case-lambda
	   ((self)
	    (socket-open self 'inet 'stream 0))
	   ((self domain)
	    (socket-open self domain 'stream 0))
	   ((self domain type)
	    (socket-open self domain type 0))
	   ((self domain type protocol)
	    (if (not (eqv? (socket-s-handle self) null))
		(socket-close self))
	    (socket-handle! self (spark.socket::socket 
				  (domain->integer domain)
				  (type->integer type)
				  (protocol->integer protocol)))
	    (if (eqv? (socket-s-handle self) null)
		(raise-sys-exception "socket-open")				
		#t))))

	;; Binds the socket to a port on the local machine.
	;; Optional arguments should be a Boolean that specifies 
	;; whether to reuse the address or not.
	(define socket-bind
	  (case-lambda
	   ((self addr)
	    (socket-bind self addr #f))
	   ((self addr reuse-addr)
	    (let ((socket-s-handle (socket-s-handle self)))		 
	      (cond
	       (reuse-addr
		(if (null? (spark.socket::setsockopt socket-s-handle
						     spark.socket::SOL-SOCKET
						     spark.socket::SO-REUSEADDR
						     1))
		    (raise-sys-exception "socket::setsockopt"))))
	      (if (null? (spark.socket::bind socket-s-handle 
					     (address->list addr)))
		  (raise-sys-exception "socket-bind"))
	      #t))))

	;; Connects to a remote host represented by the
	;; address object.
	(define (socket-connect self address)
	  (let* ((tmp-addr (ip-address (address-ip address)))
		 (alist (ip-address-list tmp-addr)))
	    (if (not (null? alist))
		(begin
		  (address-ip! address (car alist))
		  (if (eqv? (spark.socket::connect (socket-s-handle self)
						   (address->list address))
			    null)
		      (raise-sys-exception "socket-connect")))
		(raise-exception "socket-connect" "Failed to get ip-address" null)))
	  #t)

	;; Listens for incoming connections on a socket.
	;; Optional-arguments can contain one element,
	;; which should be the backlog. This defaults to 5.
	(define socket-listen
	  (case-lambda
	   ((self)
	    (socket-listen self 5))
	   ((self backlog)
	    (if (null? (spark.socket::listen (socket-s-handle self)
					     backlog))
		(raise-sys-exception "socket-listen")
		#t))))

	;; Accepts a new client connection.
	;; Returns a list that has two elements:
	;; 1. A socket object that represents the connection.
	;; 2. A address object that contains the connect information
	;; of the client.
	(define (socket-accept self)
	  (let ((r null) (client-sock null) 
		(client-addr null) (rest null))
	    (set! r (spark.socket::accept (socket-s-handle self)))
	    (if (eqv? r null)
		(raise-sys-exception "socket-accept"))
	    (set! client-sock (socket))	    
	    (socket-handle! client-sock (car r))
	    (set! rest (cdr r))
	    (if (not (eqv? rest null))
		(set! client-addr (list->address (car rest))))
	    (list client-sock client-addr)))

	;; Receives the given number of characters from a socket.
	;; The return value is a string.
	(define (socket-recv self num-bytes) 
	  (call-recv self num-bytes #f))

	;; Receives the given number of bytes from the socket
	;; and returns a list of integers that represent the bytes.
	(define (socket-recv-bytes self num-bytes)
	  (call-recv self num-bytes #t))

	;; Reads a line from the socket. 
	;; Each line input should be terminated by a \r\n.
	;; This procedure takes two optional arguments:
	;; max-size: Maximum number of bytes to read.
	;;           Defaults to 0, which imposes no limit.
	;; flag: Socket read flags. (integer).
	(define socket-recv-line
	  (case-lambda
	   ((self)
	    (socket-recv-line self 0 0))
	   ((self max-size)
	    (socket-recv-line self max-size 0))
	   ((self max-size flags)
	    (spark.socket::recv-line (socket-s-handle self)
				     max-size
				     flags))))

	(define (call-recv self num-bytes as-bytes . args)
	  (let ((ret null) (flags 0))
	    (if (not (eqv? args null))
		(set! flags (car args)))
	    (set! ret (spark.socket::recv (socket-s-handle self)
					  num-bytes
					  flags
					  as-bytes))
	    (if (eqv? ret null)
		(raise-sys-exception "socket-recv")
		ret)))

	;; Receives data over a UDP socket. The from-address
	;; specifies where to get the data from.
	;; Returns a string.
	(define (socket-recv-from self num-bytes from-address)
	  (call-recv-from self num-bytes from-address #f))

	;; Receives data as bytes over a UDP socket. The from-address
	;; specifies where to get the data from.
	;; Returns a list of integers.
	(define (socket-recv-bytes-from self num-bytes from-address)
	  (call-recv-from self num-bytes from-address #t))

	(define (call-recv-from self num-bytes from-address 
				as-bytes . args)
	  (let ((ret null) (flags 0))
	    (if (not (eqv? args null))
		(set! flags (car args)))
	    (set! ret (spark.socket::recvfrom (socket-s-handle self)
					      num-bytes
					      flags
					      from-address
					      as-bytes))
	    (if (eqv? ret null)
		(raise-sys-exception "socket-recv-from")
		ret)))

	;; Sends the given string over a socket.
	(define (socket-send self str)
	  (call-send self str))

	;; Sends raw bytes over a socket. bytes should be a list
	;; of integers. For instance, to send a \r\n sequence,
	;; use the following code:
	;; (send-bytes sock "#\r\n")
	(define (socket-send-bytes self bytes)
	  (call-send self bytes))

	;; Sends a line of text terminated by \r\n.
	(define (socket-send-line self str)
	  (socket-send self str)
	  (socket-send-bytes self #"\r\n"))

	(define (call-send self data . args)
	  (if (not (null? data))
	      (begin
		(let ((num-send 0) (flags 0))
		  (if (not (eqv? args null))
		      (set! flags (car args)))
		  (set! num-send (spark.socket::send (socket-s-handle self)
						     data
						     flags))
		  (if (eqv? num-send null)
		      (raise-sys-exception "socket-send")
		      num-send)))))

	;; Sends a string over a UDP socket. to-address specifies
	;; the destination to send to.
	(define (socket-send-to self str to-address)
	  (call-send-to self str to-address))

	;; Sends a list of bytes over a UDP socket. to-address specifies
	;; the destination to send to.
	(define (socket-send-bytes-to self bytes to-address)
	  (call-send-to self bytes to-address))

	(define (call-send-to self data to-address . args)
	  (let ((num-send 0) (flags 0))
	    (if (not (eqv? args null))
		(set! flags (car args)))
	    (set! num-send (spark.socket::sendto (socket-s-handle self)
						 data
						 flags
						 to-address))

	    (if (eqv? num-send null)
		(raise-sys-exception "socket-send-to")
		num-send)))

	;; Sets a socket option.
	;; For a list of valid names, see the (optionname->integer) procedure.
	;; The type of value depends on the option name.
	;; This can be a Boolean, integer or string. 
	;; If the option name is 'linger, value should be a list
	;; of two integers.
	(define (socket-option! self name value)
	  (let ((option-name (optionname->integer name))
		(option-value value))
	    (if (eqv? option-value #t)
		(set! option-value 1)
		(begin
		  (if (eqv? option-value #f)
		      (set! option-value 0))))
	    (if (eqv? (spark.socket::setsockopt (socket-s-handle self)
						spark.socket::SOL-SOCKET
						option-name
						option-value)
		      null)
		(raise-sys-exception "socket-set-option!")
		#t)))

	;; Returns the current value of a socket option.
	;; Return type depends on the option name.
	(define (socket-option self name)
	  (let ((option-name (optionname->integer name)) (ret null))
	    (set! ret (spark.socket::getsockopt (socket-s-handle self)
						spark.socket::SOL-SOCKET
						option-name))
	    (if (eqv? ret null)
		(raise-sys-exception "socket-get-option")
		ret)))
	
	;; Turns on/off the NONBLOCK flag.
	(define (socket-non-blocking! self flag)
	  (spark.socket::set-non-blocking (socket-s-handle self)
					  flag))
	
	;; Turns on/off the ASYNC flag.
	(define (socket-async-io! self flag)
	  (spark.socket::set-async-io (socket-s-handle self)
				      flag))

	(define (socket-non-blocking? self)
	  (spark.socket::get-non-blocking (socket-s-handle self)))
	
	(define (socket-async-io? self)
	  (spark.socket::get-async-io (socket-s-handle self)))

	;; Closes the socket handle.
	(define (socket-close self)
	  (let ((ret #t))
	    (if (not (eqv? (socket-s-handle self) null))
		(begin
		  (set! ret (spark.socket::close (socket-s-handle self)))
		  (if (eqv? ret #t)
		      (begin
			(hash-table-remove! sockets (socket-s-handle self))
			(set-socket-s-handle! self null)))))
	    ret))

	(define (socket-handle self)
	  (socket-s-handle self))

	(define (socket-handle! self handle)
	  (set-socket-s-handle! self handle)
	  (hash-table-put! sockets handle self))

	(define (socket-instance handle)
	  (let ((self (hash-table-get sockets handle null)))
	    (if (eqv? self null)
		(set! self (socket handle)))
	    self))
		  
	(define (optionname->integer name)
	  (case name
	    ((keepalive) spark.socket::SO-KEEPALIVE)
	    ((oobinline) spark.socket::SO-OOBINLINE)
	    ((rcvlowat) spark.socket::SO-RCVLOWAT)
	    ((sndlowat) spark.socket::SO-SNDLOWAT)
	    ((bsdcompat) spark.socket::SO-BSDCOMPAT)
	    ((passcred) spark.socket::SO-PASSCRED)
	    ((debug) spark.socket::SO-DEBUG)
	    ((reuseaddr) spark.socket::SO-REUSEADDR)
	    ((dontroute) spark.socket::SO-DONTROUTE)
	    ((broadcast) spark.socket::SO-BROADCAST)
	    ((sndbuf) spark.socket::SO-SNDBUF)
	    ((rcvbuf) spark.socket::SO-RCVBUF)
	    ((priority) spark.socket::SO-PRIORITY)
	    ((timestamp) spark.socket::SO-TIMESTAMP)
	    ((error) spark.socket::SO-ERROR)
	    ((acceptconn) spark.socket::SO-ACCEPTCONN)
	    ((type) spark.socket::SO-TYPE)
	    ((bindtodevice) spark.socket::SO-BINDTODEVICE)
	    ((linger) spark.socket::SO-LINGER)
	    (else (raise-exception "socket-get-option-name"
		  "Unsupported option name."))))

	(define (type->integer t)
	  (if (integer? t)
	      t
	      (begin
		(case t
		  ((stream) spark.socket::SOCK-STREAM)
		  ((dgram) spark.socket::SOCK-DGRAM)
		  ((raw) spark.socket::SOCK-RAW)
		  ((rdm) spark.socket::SOCK-RDM)
		  ((seqpacket) spark.socket::SOCK-SEQPACKET)
		  ((packet) spark.socket::SOCK-PACKET)
		  (else
		   (raise-exception "type->integer"
				    "Not a supported symbol." null))))))

	(define (protocol->integer p)
	  (if (integer? p)
	      p
	      (begin
		(case p
		  ((ip) spark.socket::IPPROTO-IP)
		  ((hopopts) spark.socket::IPPROTO-HOPOPTS)
		  ((icmp) spark.socket::IPPROTO-ICMP)
		  ((igmp) spark.socket::IPPROTO-IGMP)
		  ((ipip) spark.socket::IPPROTO-IPIP)
		  ((tcp) spark.socket::IPPROTO-TCP)
		  ((egp) spark.socket::IPPROTO-EGP)
		  ((pup) spark.socket::IPPROTO-PUP)
		  ((udp) spark.socket::IPPROTO-UDP)
		  ((idp) spark.socket::IPPROTO-IDP)
		  ((tp) spark.socket::IPPROTO-TP)
		  ((ipv6) spark.socket::IPPROTO-IPV6)
		  ((routing) spark.socket::IPPROTO-ROUTING)
		  ((fragment) spark.socket::IPPROTO-FRAGMENT)
		  ((rsvp) spark.socket::IPPROTO-RSVP)
		  ((gre) spark.socket::IPPROTO-GRE)
		  ((esp) spark.socket::IPPROTO-ESP)
		  ((ah) spark.socket::IPPROTO-AH)
		  ((icmpv6) spark.socket::IPPROTO-ICMPV6)
		  ((none) spark.socket::IPPROTO-NONE)
		  ((dstopts) spark.socket::IPPROTO-DSTOPTS)
		  ((mtp) spark.socket::IPPROTO-MTP)
		  ((encap) spark.socket::IPPROTO-ENCAP)
		  ((pim) spark.socket::IPPROTO-PIM)
		  ((comp) spark.socket::IPPROTO-COMP)
		  ((sctp) spark.socket::IPPROTO-SCTP)
		  ((raw) spark.socket::IPPROTO-RAW)
		  (else
		   (raise-exception "protocol->integer"
				    "Not a supported symbol."
				    null)))))))
	
