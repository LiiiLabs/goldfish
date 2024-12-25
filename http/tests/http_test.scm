(set! *load-path* (cons "http" *load-path*))

(import (liii check)
        (liii http))

(let1 r (http-head "https://httpbin.org")
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/")
  (check-true (real? (r 'elapsed)))
  (check (r 'reason) => "")
  (check (r 'text) => "")
  (check-true (http-ok? r)))

