(set! *load-path* (cons "http" *load-path*))

(import (liii http)
        (liii json)
        (liii list)
        (liii os)
        (liii string))

;; 定义一个函数来读取文件内容并移除行末的回车符
(define (read-and-clean-file file-path)
  (call-with-input-file file-path
    (lambda (port)
      (let loop ((line (read-line port))
                 (content ""))
        (if (eof-object? line)
            (string-trim-right content #\newline) ; 移除行末的回车符
            (loop (read-line port) (string-append content line)))))))

(define (load-silicon-cloud-api-key)
  (let* ((home (if (os-windows?)
                   (getenv "USERPROFILE") ; Windows 使用 USERPROFILE
                   (getenv "HOME")))      ; Unix/Linux 使用 HOME
         (file-path (string-append home (string (os-sep)) ".silicon_cloud")))
    (if (file-exists? file-path)          ; 直接判断文件是否存在
        (read-and-clean-file file-path)
        "请填入硅基流动的API密钥")))

(define headers
  `(
     ("Authorization" . ,(string-append "Bearer " (load-silicon-cloud-api-key)))
     ("Content-Type" . "application/json")
   )
)

(define payload
  `(
    ("model" . "Qwen/Qwen2.5-7B-Instruct")
    ("messages" . #())
    ("max_tokens" . 512)
   )
)

(define (chat payload)
  (let* ((r (http-post "https://api.siliconflow.cn/v1/chat/completions"
            :data (json->string payload)
            :headers headers)))
    (if (http-ok? r)
        (r 'text)
        (r 'status-code))))

(define questions (vector "硅基的拼音是什么？" "上一个问题是什么"))

(define (message text)
  (let1 msg `(("role" . "user") ("content" . ""))
        (json-set msg "content" text)))

(let loop ((i 0) (payload payload) (tokens 0))
  (if (< i (length questions))
      (let* ((q (json-push* payload "messages" i (message (questions i))))
             (a (chat q))
             (j (string->json a)))
          (display* "Q: " (questions i) "\n")
          (display* "A: " (json-ref* j "choices" 0 "message" "content") "\n")
          (loop (+ i 1) q (+ tokens (json-ref* j "usage" "total_tokens"))))
      (display* "Total tokens: " tokens "\n")))

