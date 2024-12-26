(set! *load-path* (cons "http" *load-path*))

(import (liii check)
        (liii http)
        (liii string))

(let1 r (http-head "https://httpbin.org")
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/")
  (check-true (real? (r 'elapsed)))
  (check (r 'reason) => "")
  (check (r 'text) => "")
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
  (check ((r 'headers) "content-length") => "9593")
  (check-true (http-ok? r)))

(let1 r (http-get "https://httpbin.org")
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8"))

(let1 r (http-get "https://httpbin.org/get"
                  :params '(("key1" . "value1") ("key2" . "value2")))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2"))

(let1 r (http-post "https://httpbin.org/post"
                  :params '(("key1" . "value1") ("key2" . "value2")))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'status-code) => 200)
      (check (r 'url) => "https://httpbin.org/post?key1=value1&key2=value2"))

(let1 r (http-post "https://httpbin.org/post"
                   :data "This is raw data")
      (check (r 'status-code) => 200)
      (display* (r 'text))
      (newline))

(check-report)

