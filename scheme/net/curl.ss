;; A wrapper over the CURL API.
;; Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal

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

(library net-curl

	 (import (exception)
		 (asserts)
		 ((prefix spark.curl:: #%spark-curl)))

	 (define (curl-env)
	   (spark.curl::curl-global-init))

	 (define (curl-env-dispose)
	   (spark.curl::curl-global-cleanup))

	 ;; Creates an easy curl object.
	 ;; Takes three optional arguments:
	 ;; 1. URL
	 ;; 2. Write callback.
	 ;; 3. Read callback.
	 ;; These callbacks should have the signature:
	 ;; (define (cb data param))
	 (define (curl . args)
	   (let ((self (spark.curl::curl-easy-init)))
	     (if (eqv? self null)
		 (raise-exception "curl" 
				  "Null handle to CURL."
				  null)
		 (begin
		   (if (not (eqv? args null))
		       (begin
			 (let ((a (car args))
			       (rest (cdr args)))
			   (curl-url! self a)
			   (if (not (eqv? rest null))
			       (begin
				 (set! a (car rest))
				 (set! rest (cdr rest))))
			   (curl-callback! self 'write a)
			   (if (not (eqv? rest null))
			       (begin
				 (set! a (car rest))))
			   (curl-callback! self 'read a))))))
	     self))
	 
	 (define (curl-dispose self)
	   (spark.curl::curl-easy-cleanup self))

	 (define (curl-perform self)
	   (spark.curl::curl-easy-perform self))

	 (define (curl-reset self)
	   (spark.curl::curl-easy-reset self))

	 ;; options

	 (define (curl-url! self s)
	   (spark.curl::curl-easy-setopt self spark.curl::CURLOPT-URL s))

	 (define (curl-callback! self type cb)
	   (let ((t (cbtype->integer type)))
	     (spark.curl::curl-easy-setopt self t cb)))

	 (define (curl-callback-param! self type p)
	   (let ((t (cbptype->integer type)))
	     (spark.curl::curl-easy-setopt self t p)))

	 (define (curl-verbose! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-VERBOSE 
					 flag))

	 (define (curl-include-header-in-body! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HEADER 
					 flag))

	 (define (curl-show-progress! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NOPROGRESS 
					 flag))

	 (define (curl-http-proxy-tunnel! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTPPROXYTUNNEL
					 flag))

	 (define (curl-tcp-nodelay! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TCP_NODELAY
					 flag))

	 (define (curl-fail-on-error! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FAILONERROR
					 flag))

	 (define (curl-http-autoreferer! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-AUTOREFERER
					 flag))
	 
	 (define (curl-http-followlocation! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FOLLOWLOCATION
					 flag))

	 (define (curl-http-unrestricted-auth! self flag)
	   (spark.curl::curl-easy-setopt self
					 spark.curl::CURLOPT-UNRESTRICTED-AUTH
					 flag))

	 (define (curl-http-post-301! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-POST301
					 flag))

	 (define (curl-http-use-post! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-POST
					 flag))

	 (define (curl-http-use-get! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTPGET
					 flag))

	 (define (curl-http-use-put! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PUT
					 flag))

	 (define (curl-http-cookie-session! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COOKIESESSION
					 flag))

	 (define (curl-http-ignore-content-length! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-IGNORE-CONTENT-LENGTH
					 flag))

	 (define (curl-http-do-content-decoding! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTP-CONTENT-DECODING
					 flag))

	 (define (curl-http-do-transfer-decoding! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTP-TRANSFER-DECODING
					 flag))

	 (define (curl-ftp-dirlistonly! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-DIRLISTONLY
					 flag))

	 (define (curl-ftp-append! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-APPEND
					 flag))

	 (define (curl-ftp-use-eprt! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-USE-EPRT
					 flag))

	 (define (curl-ftp-use-epsv! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-USE-EPSV
					 flag))

	 (define (curl-ftp-create-missing-dirs! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-CREATE-MISSING-DIRS
					 flag))

	 (define (curl-ftp-skip-pasv-ip! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-SKIP-PASV-IP
					 flag))

	 (define (curl-proxy-transfer-mode! self flag)
	   (spark.curl::curl-easy-setopt self
					 spark.curl::CURLOPT-PROXY-TRANSFER-MODE
					 flag))

	 (define (curl-unix-newlines-to-crlf! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CRLF
					 flag))

	 (define (curl-no-body! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NOBODY
					 flag))

	 (define (curl-prepare-for-upload! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-UPLOAD
					 flag))

	 (define (curl-fresh-connect! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FRESH-CONNECT
					 flag))

	 (define (curl-forbid-reuse! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FORBID-REUSE
					 flag))

	 (define (curl-connect-only! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CONNECT-ONLY
					 flag))

	 (define (curl-ssl-use-default-engine! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLENGINE-DEFAULT
					 flag))

	 (define (curl-ssl-verify-peer! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSL-VERIFYPEER
					 flag))

	 (define (curl-ssl-verify-host! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSL-VERIFYHOST
					 flag))

	 (define (curl-ssl-sessionid-cache! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSL-SESSIONID-CACHE
					 flag))

	 (define (curl-no-signal! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NOSIGNAL
					 flag))

	 (define (curl-try-to-get-file-time! self flag)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FILETIME
					 flag))

	 (define (curl-proxy! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PROXY
					 s))

	 (define (curl-interface! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-INTERFACE
					 s))

	 (define (curl-netrc-file! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NETRC-FILE
					 s))

	 (define (curl-user-password! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-USERPWD
					 s))

	 (define (curl-proxy-user-password! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PROXYUSERPWD
					 s))

	 (define (curl-http-accept-encoding! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-ENCODING
					 s))

	 (define (curl-http-postfields! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COPYPOSTFIELDS
					 s))

	 (define (curl-http-referer! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-REFERER
					 s))
	 
	 (define (curl-http-user-agent! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-USERAGENT
					 s))

	 (define (curl-http-cookie! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COOKIE
					 s))

	 (define (curl-http-cookie-file! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COOKIEFILE
					 s))

	 (define (curl-http-cookie-jar! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COOKIEJAR
					 s))

	 (define (curl-http-cookie-list! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-COOKIELIST
					 s))

	 (define (curl-ftp-port! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTPPORT
					 s))

	 (define (curl-ftp-alternative-to-user! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-ALTERNATIVE-TO-USER
					 s))

	 (define (curl-ftp-account! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-ACCOUNT
					 s))

	 (define (curl-range! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-RANGE
					 s))

	 (define (curl-custom-request! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CUSTOMREQUEST
					 s))

	 (define (curl-ssl-cert! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLCERT
					 s))

	 (define (curl-ssl-cert-type! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLCERTTYPE
					 s))

	 (define (curl-ssl-key! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLKEY
					 s))

	 (define (curl-ssl-key-type! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLKEYTYPE
					 s))

	 (define (curl-ssl-key-password! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-KEYPASSWD
					 s))

	 (define (curl-ssl-engine! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLENGINE
					 s))

	 (define (curl-ssl-ca-info! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CAINFO
					 s))

	 (define (curl-ssl-ca-path! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CAPATH 
					 s))

	 (define (curl-ssl-random-seed-file! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-RANDOM-FILE
					 s))	

	 (define (curl-ssl-egd-seed! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-EGDSOCKET
					 s))					

	 (define (curl-ssl-cipher-list! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSL-CIPHER-LIST
					 s))	

	 (define (curl-kerberos-security-level! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-KRBLEVEL
					 s))	

	 (define (curl-ssh-host-public-key-md6! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSH-HOST-PUBLIC-KEY-MD5
					 s))	

	 (define (curl-ssh-public-key-file! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSH-PUBLIC-KEYFILE
					 s))	

	 (define (curl-ssh-private-key-file! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSH-PRIVATE-KEYFILE
					 s))	

	 (define (curl-proxy-port! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PROXYPORT
					 i))	

	 (define (curl-proxy-type! self t)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PROXYTYPE
					 (proxytype->integer t)))

	 (define (curl-local-port! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-LOCALPORT
					 i))	

	 (define (curl-local-port-range! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-LOCALPORTRANGE
					 i))	

	 (define (curl-dns-cache-timeout! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-DNS-CACHE-TIMEOUT
					 i))	

	 (define (curl-buffer-size! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-BUFFERSIZE
					 i))	

	 (define (curl-port! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PORT
					 i))	

	 (define (curl-netrc! self n)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NETRC
					 (netrc->integer n)))

	 (define (curl-http-auth! self a)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTPAUTH
					 (httpauth->integer a)))

	 (define (curl-proxy-auth! self a)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-LOCALPORT
					 (httpauthlist->integerlist a)))

	 (define (curl-http-max-redirs! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAXREDIRS
					 i))

	 (define (curl-http-version! self v)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAXREDIRS
					 (httpversion->integer v)))

	 (define (curl-ftp-response-timeout! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-RESPONSE-TIMEOUT
					 i))

	 (define (curl-ftp-use-ssl! self s)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-USE-SSL
					 (ssllevel->integer s)))

	 (define (curl-ftp-ssl-auth! self a)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTPSSLAUTH
					 (sslauth->integer a)))

	 (define (curl-ftp-ssl-ccc! self c)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-RESPONSE-TIMEOUT
					 (sslccc->integer c)))

	 (define (curl-ftp-file-method! self m)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-FTP-FILEMETHOD
					 (ftpfilemethod->integer m)))

	 (define (curl-resume-from! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-RESUME-FROM-LARGE
					 i))

	 (define (curl-expected-in-file-size! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-INFILESIZE
					 i))

	 (define (curl-max-file-size! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAXFILESIZE
					 i))

	 (define (curl-time-condition! self c)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TIMECONDITION
					 (timecondition->integer c)))

	 (define (curl-time-value! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TIMEVALUE
					 i))

	 (define (curl-time-out-secs! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TIMEOUT
					 i))

	 (define (curl-time-out-milli-secs! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TIMEOUT-MS
					 i))

	 (define (curl-low-speed-bytes-per-second! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-LOW-SPEED-LIMIT
					 i))

	 (define (curl-tolerate-low-speed! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-LOW-SPEED-TIME
					 i))

	 (define (curl-max-connects! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAXCONNECTS
					 i))

	 (define (curl-connect-timeout-secs! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CONNECTTIMEOUT
					 i))

	 (define (curl-connect-timeout-milli-secs! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-CONNECTTIMEOUT-MS
					 i))

	 (define (curl-ipresolve! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-IPRESOLVE
					 (ipresolve->integer i)))

	 (define (curl-ssl-version! self v)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSLVERSION
					 (sslversion->integer v)))

	 (define (curl-file-perms! self perms)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NEW-FILE-PERMS
					 perms))

	 (define (curl-directory-perms! self perms)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-NEW-DIRECTORY-PERMS
					 perms))

	 (define (curl-max-send-speed! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAX-SEND-SPEED-LARGE
					 i))

	 (define (curl-max-recv-speed! self i)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-MAX-RECV-SPEED-LARGE
					 i))
	 
	 (define (curl-http-post! self post-data-index)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTPPOST
					 post-data-index))

	 (define (curl-http-header! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTPHEADER
					 lst))

	 (define (curl-http-200-aliases! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-HTTP200ALIASES
					 lst))

	 (define (curl-ftp-quote! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-QUOTE
					 lst))

	 (define (curl-ftp-post-quote! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-POSTQUOTE
					 lst))

	 (define (curl-ftp-pre-quote! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-PREQUOTE
					 lst))

	 (define (curl-telnet-options! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-TELNETOPTIONS
					 lst))

	 (define (curl-ssh-auth-types! self lst)
	   (spark.curl::curl-easy-setopt self 
					 spark.curl::CURLOPT-SSH-AUTH-TYPES
					 (sshauthtypes->integerlist lst)))

	 (define (curl-info! self inf)
	   (spark.curl::curl-easy-getinfo self 
					  (infotype->integer inf)))

	 (define (curl-last-error)
	   (spark.curl::curl-easy-strerror))

	 ;; multi

	 (define (multi-curl)
	   (let ((self (spark.curl::curl-multi-init)))
	     (if (eqv? self null)
		 (raise-exception "init-multi-curl"
				  "Null handle to multi-curl" null))
	     self))

	 (define (multi-curl-dispose self)
	   (spark.curl::curl-multi-cleanup))

	 (define (multi-curl-perform self)
	   (spark.curl::curl-multi-perform))

	 (define (multi-curl-add self curl)
	   (spark.curl::curl-multi-add-handle self curl))

	 (define (multi-curl-remove self curl)
	   (spark.curl::curl-multi-remove-handle self curl))

	 (define (multi-curl-last-error self)
	   (spark.curl::curl-multi-strerror self))

	 (define (multi-curl-timeout self)
	   (spark.curl::curl-multi-timeout self))	    

	 (define (cbtype->integer type)
	   (case type
	     ((write) spark.curl::CURLOPT-WRITEFUNCTION)
	     ((read) spark.curl::CURLOPT-READFUNCTION)
	     ((progress) spark.curl::CURLOPT-PROGRESSFUNCTION)
	     ((header) spark.curl::CURLOPT-HEADERFUNCTION)
	     ((debug) spark.curl::CURLOPT-DEBUGFUNCTION)
	     (else (raise-exception "cbtype->integer"
				    "Unsupported callback type."
				    null))))

	 (define (cbptype->integer type)
	   (case type
	     ((write) spark.curl::CURLOPT-WRITEDATA)
	     ((read) spark.curl::CURLOPT-READDATA)
	     ((progress) spark.curl::CURLOPT-PROGRESSDATA)
	     ((header) spark.curl::CURLOPT-HEADERDATA)
	     ((debug) spark.curl::CURLOPT-DEBUGDATA)
	     (else (raise-exception "cbptype->integer"
				    "Unsupported callback type."
				    null))))

	 (define (proxytype->integer p)
	   (case p
	     ((http) spark.curl::CURLPROXY-HTTP)
	     ((socks4) spark.curl::CURLPROXY-SOCKS4)
	     ((socks5) spark.curl::CURLPROXY-SOCKS5)
	     ((socks4a) spark.curl::CURLPROXY-SOCKS4A)
	     ((socks5-hostname) spark.curl::CURLPROXY-SOCKS5-HOSTNAME)
	     (else (raise-exception "proxytype->integer"
				    "Unsupported proxytype."
				    null))))

	 (define (netrc->integer n)
	   (case n
	     ((optional) spark.curl::CURL-NETRC-OPTIONAL)
	     ((ignored) spark.curl::CURL-NETRC-IGNORED)
	     ((required) spark.curl::CURL-NETRC-REQUIRED)
	     (else (raise-exception "netrc->integer"
				    "Unsupported netrc."
				    null))))

	 (define (httpauth->integer h)
	   (case h
	     ((basic) spark.curl::CURLAUTH-BASIC)
	     ((digest) spark.curl::CURLAUTH-DIGEST)
	     ((gssnegotiate) spark.curl::CURLAUTH-GSSNEGOTIATE)
	     ((ntlm) spark.curl::CURLAUTH-NTLM)
	     ((any) spark.curl::CURLAUTH-ANY)
	     ((any-safe) spark.curl::CURLAUTH-ANYSAFE)
	     (else (raise-exception "httpauth->integer"
				    "Unsupported httpauth."
				    null))))

	 (define (httpauthlist->integerlist lst)
	   (let ((h (car lst))
		 (lst (cdr lst))
		 (ret (list)))
	     (let loop ()
	       (set! lst (append lst (list (httpauth->integer h))))
	       (if (not (eqv? lst null))
		   (begin
		     (set! h (car lst))
		     (set! lst (cdr lst))
		     (loop))))
	     ret))

	 (define (httpversion->integer v)
	   (case v
	     ((none) spark.curl::CURL-HTTP-VERSION-NONE)
	     ((1.0) spark.curl::CURL-HTTP-VERSION-1-0)
	     ((1.1) spark.curl::CURL-HTTP-VERSION-1-1)
	     (else (raise-exception "httpversion->integer"
				    "Unsupported httpversion."
				    null))))

	 (define (ssllevel->integer s)
	   (case s
	     ((none) spark.curl::CURLUSESSL-NONE)
	     ((try) spark.curl::CURLUSESSL-TRY)
	     ((control) spark.curl::CURLUSESSL-CONTROL)
	     ((all) spark.curl::CURLUSESSL-ALL)
	     (else (raise-exception "ssllevel->integer"
				    "Unsupported ssllevel."
				    null))))

	 (define (sslauth->integer s)
	   (case s
	     ((default) spark.curl::CURLFTPAUTH-DEFAULT)
	     ((ssl) spark.curl::CURLFTPAUTH-SSL)
	     ((tls) spark.curl::CURLFTPAUTH-TLS)
	     (else (raise-exception "sslauth->integer"
				    "Unsupported sslauth."
				    null))))

	 (define (sslccc->integer s)
	   (case s
	     ((none) spark.curl::CURLFTPSSL-CCC-NONE)
	     ((active) spark.curl::CURLFTPSSL-CCC-ACTIVE)
	     ((passive) spark.curl::CURLFTPSSL-CCC-PASSIVE)
	     (else (raise-exception "sslccc->integer"
				    "Unsupported sslccc."
				    null))))

	 (define (ftpfilemethod->integer s)
	   (case s
	     ((multicwd) spark.curl::CURLFTPMETHOD-MULTICWD)
	     ((nocwd) spark.curl::CURLFTPMETHOD-NOCWD)
	     ((singlecwd) spark.curl::CURLFTPMETHOD-SINGLECWD)
	     (else (raise-exception "ftpfilemethod->integer"
				    "Unsupported ftpmethod."
				    null))))

	 (define (timecondition->integer t)
	   (case t
	     ((if-mod-since) spark.curl::CURL-TIMECOND-IFMODSINCE)
	     ((if-unmod-since) spark.curl::CURL-TIMECOND-IFUNMODSINCE)
	     (else (raise-exception "timecondition->integer"
				    "Unsupported timecondition."
				    null))))

	 (define (ipresolve->integer i)
	   (case i
	     ((whatever) spark.curl::CURL-IPRESOLVE-WHATEVER)
	     ((v4) spark.curl::CURL-IPRESOLVE-V4)
	     ((v6) spark.curl::CURL-IPRESOLVE-V6)
	     (else (raise-exception "ipresolve->integer"
				    "Unsupported ipresolve."
				    null))))

	 (define (sslversion->integer v)
	   (case v
	     ((default) spark.curl::CURL-SSLVERSION-DEFAULT)
	     ((tls-v1) spark.curl::CURL-SSLVERSION-TLSv1)
	     ((ssl-v2) spark.curl::CURL-SSLVERSION-SSLv2)
	     ((ssl-v3) spark.curl::CURL-SSLVERSION-SSLv3)
	     (else (raise-exception "sslversion->integer"
				    "Unsupported sslversion."
				    null))))

	 (define (sshauthtype->integer s)
	   (case s
	     ((public-key) spark.curl::CURLSSH-AUTH-PUBLICKEY)
	     ((password) spark.curl::CURLSSH-AUTH-PASSWORD)
	     ((host) spark.curl::CURLSSH-AUTH-HOST)
	     ((keyboard) spark.curl::CURLSSH-AUTH-KEYBOARD)
	     ((any) spark.curl::CURLSSH-AUTH-ANY)
	     (else (raise-exception "shauthtype->integer"
				    "Unsupported sshauthtype."
				    null))))

	 (define (sshauthtypes->integerlist lst)
	   (let ((s (car lst))
		 (lst (cdr lst))
		 (ret (list)))
	     (let loop ()
	       (set! ret (append ret (list (sshauthtype->integer s))))
	       (if (not (eqv? lst null))
		   (begin
		     (set! s (car lst))
		     (set! lst (cdr lst))
		     (loop))))
	     ret))		

	 (define (infotype->integer i)
	   (case i
	     ((effective-url) spark.curl::CURLINFO-EFFECTIVE-URL)
	     ((response-code) spark.curl::CURLINFO-RESPONSE-CODE)
	     ((http-connectcode) spark.curl::CURLINFO-HTTP-CONNECTCODE)
	     ((filetime) spark.curl::CURLINFO-FILETIME)
	     ((total-time) spark.curl::CURLINFO-TOTAL-TIME)
	     ((namelookup-time) spark.curl::CURLINFO-NAMELOOKUP-TIME)
	     ((connect-time) spark.curl::CURLINFO-CONNECT-TIME)
	     ((pretransfer-time) spark.curl::CURLINFO-PRETRANSFER-TIME)
	     ((starttransfer-time) spark.curl::CURLINFO-STARTTRANSFER-TIME)
	     ((redirect-time) spark.curl::CURLINFO-REDIRECT-TIME)
	     ((redirect-count) spark.curl::CURLINFO-REDIRECT-COUNT)
	     ((size-upload) spark.curl::CURLINFO-SIZE-UPLOAD)
	     ((size-download) spark.curl::CURLINFO-SIZE-DOWNLOAD)
	     ((speed-download) spark.curl::CURLINFO-SPEED-DOWNLOAD)
	     ((speed-upload) spark.curl::CURLINFO-SPEED-UPLOAD)
	     ((header-size) spark.curl::CURLINFO-HEADER-SIZE)
	     ((request-size) spark.curl::CURLINFO-REQUEST-SIZE)
	     ((ssl-verifyresult) spark.curl::CURLINFO-SSL-VERIFYRESULT)
	     ((ssl-engines) spark.curl::CURLINFO-SSL-ENGINES)
	     ((content-length-download) spark.curl::CURLINFO-CONTENT-LENGTH-DOWNLOAD)
	     ((content-type) spark.curl::CURLINFO-CONTENT-TYPE)
	     ((private) spark.curl::CURLINFO-PRIVATE)
	     ((httpauth-avail) spark.curl::CURLINFO-HTTPAUTH-AVAIL)
	     ((proxyauth-avail) spark.curl::CURLINFO-PROXYAUTH-AVAIL)
	     ((os-errno) spark.curl::CURLINFO-OS-ERRNO)
	     ((num-connects) spark.curl::CURLINFO-NUM-CONNECTS)
	     ((cookielist) spark.curl::CURLINFO-COOKIELIST)
	     ((ftp-entry-path) spark.curl::CURLINFO-FTP-ENTRY-PATH)
	     (else (raise-exception "infotype->integer"
				    "Unsupported infotype."
				    null))))

	 (export curl-env
		 curl-env-dispose
		 curl
		 curl-dispose
		 curl-perform 
		 curl-reset
		 curl-verbose!
		 curl-url!
		 curl-callback!
		 curl-callback-param!
		 curl-include-header-in-body! 
		 curl-show-progress!
		 curl-fail-on-error!
		 curl-http-proxy-tunnel!
		 curl-tcp-nodelay!
		 curl-http-autoreferer!
		 curl-http-followlocation!
		 curl-http-unrestricted-auth! 
		 curl-http-post-301!
		 curl-http-use-post! 
		 curl-http-use-put!
		 curl-http-use-get!
		 curl-http-cookie-session! 
		 curl-http-ignore-content-length!
		 curl-http-do-content-decoding!
		 curl-http-do-transfer-decoding!
		 curl-ftp-dirlistonly! 
		 curl-ftp-append!
		 curl-ftp-use-eprt! 
		 curl-ftp-use-epsv!
		 curl-ftp-create-missing-dirs! 
		 curl-ftp-skip-pasv-ip!
		 curl-proxy-transfer-mode!
		 curl-unix-newlines-to-crlf!
		 curl-no-body! 
		 curl-prepare-for-upload! 
		 curl-fresh-connect!
		 curl-forbid-reuse!
		 curl-connect-only! 
		 curl-ssl-use-default-engine!
		 curl-ssl-verify-peer! 
		 curl-ssl-verify-host!
		 curl-ssl-sessionid-cache!
		 curl-no-signal!
		 curl-try-to-get-file-time!
		 curl-proxy! 
		 curl-interface!
		 curl-netrc-file!
		 curl-user-password! 
		 curl-proxy-user-password!
		 curl-http-accept-encoding! 
		 curl-http-postfields!
		 curl-http-referer! 
		 curl-http-user-agent! 
		 curl-http-cookie!
		 curl-http-cookie-file! 
		 curl-http-cookie-jar!
		 curl-http-cookie-list! 
		 curl-ftp-port!
		 curl-ftp-alternative-to-user!
		 curl-ftp-account! 
		 curl-range! 
		 curl-custom-request!
		 curl-ssl-cert!
		 curl-ssl-cert-type!
		 curl-ssl-key!
		 curl-ssl-key-type! 
		 curl-ssl-key-password! 
		 curl-ssl-engine! 
		 curl-ssl-ca-info!
		 curl-ssl-ca-path!
		 curl-ssl-random-seed-file!
		 curl-ssl-egd-seed! 
		 curl-ssl-cipher-list!
		 curl-kerberos-security-level!
		 curl-ssh-host-public-key-md6!
		 curl-ssh-public-key-file!
		 curl-ssh-private-key-file! 
		 curl-proxy-port!
		 curl-proxy-type! 
		 curl-local-port!
		 curl-local-port-range! 
		 curl-dns-cache-timeout!
		 curl-buffer-size! 
		 curl-port! 
		 curl-netrc!
		 curl-http-auth!
		 curl-proxy-auth! 
		 curl-http-max-redirs!
		 curl-http-version!
		 curl-ftp-response-timeout! 
		 curl-ftp-use-ssl!
		 curl-ftp-ssl-auth!
		 curl-ftp-ssl-ccc! 
		 curl-ftp-file-method! 
		 curl-resume-from! 
		 curl-expected-in-file-size!
		 curl-max-file-size! 
		 curl-time-condition! 
		 curl-time-value! 
		 curl-time-out-secs!
		 curl-time-out-milli-secs!
		 curl-low-speed-bytes-per-second!
		 curl-tolerate-low-speed!
		 curl-max-connects!
		 curl-connect-timeout-secs!
		 curl-connect-timeout-milli-secs!
		 curl-ipresolve!
		 curl-ssl-version! 
		 curl-file-perms!
		 curl-directory-perms!
		 curl-max-send-speed!
		 curl-max-recv-speed!
		 curl-http-post!
		 curl-http-header!
		 curl-http-200-aliases! 
		 curl-ftp-quote!
		 curl-ftp-post-quote!
		 curl-ftp-pre-quote!
		 curl-telnet-options! 
		 curl-ssh-auth-types! 
		 curl-info!
		 curl-last-error
		 multi-curl multi-curl-dispose
		 multi-curl-perform
		 multi-curl-add multi-curl-remove
		 multi-curl-last-error
		 multi-curl-timeout))


