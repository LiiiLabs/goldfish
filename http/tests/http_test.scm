(set! *load-path* (cons "http" *load-path*))

(import (liii check)
        (liii http))

(let1 r (http-head "https://httpbin.org")
  (check (r 'status-code) => 200))

