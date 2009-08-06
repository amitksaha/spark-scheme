;; Represents an IP address.
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

(library net-address

	 (import ((prefix spark.socket:: #%spark-socket))
		 (exception)
		 (asserts))

	 (export address? address address->string
		 address->list list->address
		 address-family! address-family
		 address-port! address-port
		 address-ip! address-ip
		 ip-address ip-address-name
		 ip-address-aliases ip-address-type
		 ip-address-length ip-address-list
		 domain->integer)

	 (define-struct address-s (ip port family))
	 
	 ;; Creates and initializes a new address object.
	 ;; Takes three optional arguments: the IP address, port
	 ;; and address family. Address family can be any valid
	 ;; AF-*** constant, and defaults to AF-INET.
	 ;; Returns the new address object.
	 (define address
	   (case-lambda
	    (()
	     (address null 0 spark.socket::AF-INET))
	    ((ip)
	     (address ip 0 spark.socket::AF-INET))
	    ((ip port)
	     (address ip port spark.socket::AF-INET))
	    ((ip port family)
	     (make-address-s ip
			     port
			     family))))
	 
	 ;; Returns a list that contains the given address objects
	 ;; members in the following order:
	 ;; (family port IP)
	 (define (address->list self)
	   (list (address-s-family self)
		 (address-s-port self)
		 (address-s-ip self)))

	 ;; Returns a string representation of the address in the
	 ;; following format:
	 ;; ip:port
	 (define (address->string self)
	   (let ((out (open-output-string)))
	     (fprintf out "~a:~a"
		      (address-s-ip self)
		      (address-s-port self))
	     (get-output-string out)))

	 ;; Creates a new address object using the first 3
	 ;; values of the given list. The list should be in
	 ;; the following format:
	 ;; (family port IP)
	 (define (list->address l)
	   (let ((rest ()) (ip null) (family 0) (port 0))
	     (set! family (car l))
	     (set! rest (cdr l))
	     (set! port (car rest))
	     (set! ip (car (cdr rest)))
	     (address ip port family)))

	 
	 (define (address? self)
	   (address-s? self))

	 (define (address-family! self f)
	   (set-address-s-family! self f))

	 (define (address-family self)
	   (address-s-family self))

	 (define (address-port! self p)
	   (set-address-s-port! self p))

	 (define (address-port self)
	   (address-s-port self))

	 (define (address-ip! self ip)
	   (set-address-s-ip! self ip))

	 (define (address-ip self)
	   (address-s-ip self))

	 (define-struct ip-address-s (name aliases type length addrs))
	 
	 (define (ip-address host-name)
	   (let ((addr (spark.socket::gethostbyname host-name)))
	     (if (not (null? addr))
		 (begin
		   (let ((ret (make-ip-address-s null (list) 0 4 (list))))
		     (set-ip-address-s-name! ret (car addr))
		     (set! addr (cdr addr))
		     (set-ip-address-s-aliases! ret (car addr))
		     (set! addr (cdr addr))
		     (set-ip-address-s-type! ret (car addr))
		     (set! addr (cdr addr))
		     (set-ip-address-s-length! ret (car addr))
		     (set! addr (cdr addr))
		     (set-ip-address-s-addrs! ret (car addr))
		     ret))
		 null)))

	 (define (ip-address-name ipaddr)
	   (ip-address-s-name ipaddr))

	 (define (ip-address-aliases ipaddr)
	   (ip-address-s-aliases ipaddr))

	 (define (ip-address-type ipaddr)
	   (integer->domain (ip-address-s-type ipaddr)))

	 (define (ip-address-length ipaddr)
	   (ip-address-s-length ipaddr))

	 (define (ip-address-list ipaddr)
	   (ip-address-s-addrs ipaddr))

	 (define (domain->integer d)
	   (if (integer? d)
	       d
	       (begin
		 (case d
		   ((inet) spark.socket::PF-INET)
		   ((unspec) spark.socket::PF-UNSPEC)
		   ((local) spark.socket::PF-LOCAL)
		   ((unix) spark.socket::PF-UNIX)
		   ((file) spark.socket::PF-FILE)
		   ((ax25) spark.socket::PF-AX25)
		   ((ipx) spark.socket::PF-IPX)
		   ((appletalk) spark.socket::PF-APPLETALK)
		   ((netrom) spark.socket::PF-NETROM)
		   ((bridge) spark.socket::PF-BRIDGE)
		   ((atmpvc) spark.socket::PF-ATMPVC)
		   ((x25) spark.socket::PF-X25)
		   ((inet6) spark.socket::PF-INET6)
		   ((rose) spark.socket::PF-ROSE)
		   ((decnet) spark.socket::PF-DECNET)
		   ((netbeui) spark.socket::PF-NETBEUI)
		   ((security) spark.socket::PF-SECURITY)
		   ((key) spark.socket::PF-KEY)
		   ((netlink) spark.socket::PF-NETLINK)
		   ((route) spark.socket::PF-ROUTE)
		   ((packet) spark.socket::PF-PACKET)
		   ((ash) spark.socket::PF-ASH)
		   ((econet) spark.socket::PF-ECONET)
		   ((atmsvc) spark.socket::PF-ATMSVC)
		   ((sna) spark.socket::PF-SNA)
		   ((irda) spark.socket::PF-IRDA)
		   ((pppox) spark.socket::PF-PPPOX)
		   ((wanpipe) spark.socket::PF-WANPIPE)
		   ((bluetooth) spark.socket::PF-BLUETOOTH)
		   ((af-unspec) spark.socket::AF-UNSPEC)
		   ((af-local) spark.socket::AF-LOCAL)
		   ((af-unix) spark.socket::AF-UNIX)
		   ((af-file) spark.socket::AF-FILE)
		   ((af-inet) spark.socket::AF-INET)
		   ((af-ax25) spark.socket::AF-AX25)
		   ((af-ipx) spark.socket::AF-IPX)
		   ((af-appletalk) spark.socket::AF-APPLETALK)
		   ((af-netrom) spark.socket::AF-NETROM)
		   ((af-bridge) spark.socket::AF-BRIDGE)
		   ((af-atmpvc) spark.socket::AF-ATMPVC)
		   ((af-x25) spark.socket::AF-X25)
		   ((af-inet6) spark.socket::AF-INET6)
		   ((af-rose) spark.socket::AF-ROSE)
		   ((af-decnet) spark.socket::AF-DECNET)
		   ((af-netbeui) spark.socket::AF-NETBEUI)
		   ((af-security) spark.socket::AF-SECURITY)
		   ((af-key) spark.socket::AF-KEY)
		   ((af-netlink) spark.socket::AF-NETLINK)
		   ((af-route) spark.socket::AF-ROUTE)
		   ((af-packet) spark.socket::AF-PACKET)
		   ((af-ash) spark.socket::AF-ASH)
		   ((af-econet) spark.socket::AF-ECONET)
		   ((af-atmsvc) spark.socket::AF-ATMSVC)
		   ((af-sna) spark.socket::AF-SNA)
		   ((af-irda) spark.socket::AF-IRDA)
		   ((af-pppox) spark.socket::AF-PPPOX)
		   ((af-wanpipe) spark.socket::AF-WANPIPE)
		   ((af-bluetooth) spark.socket::AF-BLUETOOTH)
		   (else
		    (raise-exception "domain->integer" 
				     "Not a supported constant." null))))))

	 (define (integer->domain i)
	   (cond
	    ((= i spark.socket::PF-INET) 'inet)
	    ((= i spark.socket::PF-UNSPEC) 'unspec)
	    ((= i spark.socket::PF-LOCAL) 'local) 
	    ((= i spark.socket::PF-UNIX) 'unix) 
	    ((= i spark.socket::PF-FILE) 'file) 
	    ((= i spark.socket::PF-AX25) 'ax25) 
	    ((= i spark.socket::PF-IPX) 'ipx) 
	    ((= i spark.socket::PF-APPLETALK) 'appletalk) 
	    ((= i spark.socket::PF-NETROM) 'netrom) 
	    ((= i spark.socket::PF-BRIDGE) 'bridge) 
	    ((= i spark.socket::PF-ATMPVC) 'atmpvc)
	    ((= i spark.socket::PF-X25) 'x25)
	    ((= i spark.socket::PF-INET6) 'inet6) 
	    ((= i spark.socket::PF-ROSE) 'rose) 
	    ((= i spark.socket::PF-DECNET) 'decnet) 
	    ((= i spark.socket::PF-NETBEUI) 'netbeui) 
	    ((= i spark.socket::PF-SECURITY) 'security) 
	    ((= i spark.socket::PF-KEY) 'key) 
	    ((= i spark.socket::PF-NETLINK) 'netlink) 
	    ((= i spark.socket::PF-ROUTE) 'route) 
	    ((= i spark.socket::PF-PACKET) 'packet) 
	    ((= i spark.socket::PF-ASH) 'ash) 
	    ((= i spark.socket::PF-ECONET) 'econet) 
	    ((= i spark.socket::PF-ATMSVC) 'atmsvc) 
	    ((= i spark.socket::PF-SNA) 'sna) 
	    ((= i spark.socket::PF-IRDA) 'irda) 
	    ((= i spark.socket::PF-PPPOX) 'pppox) 
	    ((= i spark.socket::PF-WANPIPE) 'wanpipe) 
	    ((= i spark.socket::PF-BLUETOOTH) 'bluetooth) 
	    ((= i spark.socket::AF-UNSPEC) 'af-unspec) 
	    ((= i spark.socket::AF-LOCAL) 'af-local) 
	    ((= i spark.socket::AF-UNIX) 'af-unix) 
	    ((= i spark.socket::AF-FILE) 'af-file) 
	    ((= i spark.socket::AF-INET) 'af-inet) 
	    ((= i spark.socket::AF-AX25) 'af-ax25) 
	    ((= i spark.socket::AF-IPX) 'af-ipx) 
	    ((= i spark.socket::AF-APPLETALK) 'af-appletalk) 
	    ((= i spark.socket::AF-NETROM) 'af-netrom) 
	    ((= i spark.socket::AF-BRIDGE) 'af-bridge) 
	    ((= i spark.socket::AF-ATMPVC) 'af-atmpvc) 
	    ((= i spark.socket::AF-X25) 'af-x25) 
	    ((= i spark.socket::AF-INET6) 'af-inet6) 
	    ((= i spark.socket::AF-ROSE) 'af-rose) 
	    ((= i spark.socket::AF-DECNET) 'af-decnet) 
	    ((= i spark.socket::AF-NETBEUI) 'af-netbeui) 
	    ((= i spark.socket::AF-SECURITY) 'af-security) 
	    ((= i spark.socket::AF-KEY) 'af-key) 
	    ((= i spark.socket::AF-NETLINK) 'af-netlink) 
	    ((= i spark.socket::AF-ROUTE) 'af-route) 
	    ((= i spark.socket::AF-PACKET) 'af-packet) 
	    ((= i spark.socket::AF-ASH) 'af-ash) 
	    ((= i spark.socket::AF-ECONET) 'af-econet) 
	    ((= i spark.socket::AF-ATMSVC) 'af-atmsvc) 
	    ((= i spark.socket::AF-SNA) 'af-sna) 
	    ((= i spark.socket::AF-IRDA) 'af-irda) 
	    ((= i spark.socket::AF-PPPOX) 'af-pppox) 
	    ((= i spark.socket::AF-WANPIPE) 'af-wanpipe) 
	    ((= i spark.socket::AF-BLUETOOTH) 'af-bluetooth) 
	    (else
	     'unknown))))






