(import "HTTP")

(define conn (http-connect "www.sics.se" 80))
(http-close conn)
