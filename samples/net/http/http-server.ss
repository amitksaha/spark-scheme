(import (net)
	(reactor)
	(http)
	(aura))

(define httpd (web-server (list 'port 8080)))
(web-server-start httpd)
(web-server-stop httpd)