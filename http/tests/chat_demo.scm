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
    ("model" . "Qwen/Qwen2.5-Coder-7B-Instruct")
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

(define questions
  #("唐宋八大家是哪八位"
    "请按照顺序返回上一个回答中的第五位"
    "上一个问题的回答一共几个汉字？"
    "介绍他的生平和作品"))

(define (message text)
  (let1 msg `(("role" . "user") ("content" . ""))
        (json-set msg "content" text)))

(define (json-escape-string str)
  (define (escape-char c)
    (case c
      ((#\\) "\\\\")  ; 反斜杠
      ((#\") "\\\"")  ; 双引号
      ((#\newline) "\\n")  ; 换行符
      ((#\tab) "\\t")  ; 制表符
      ((#\return) "\\r")  ; 回车符
      ((#\backspace) "\\b")  ; 退格符
      ((#\formfeed) "\\f")  ; 换页符
      (else (string c))))  ; 其他字符保持不变

  (let loop ((chars (string->list str))  ; 将字符串转换为字符列表
             (result ""))  ; 初始化结果字符串
    (if (null? chars)
        result  ; 如果字符列表为空，返回结果
        (loop (cdr chars)  ; 递归处理剩余的字符
              (string-append result (escape-char (car chars)))))))  ; 转义当前字符并追加到结果中

(let loop ((i 0) (payload payload) (tokens 0))
  (if (< i (length questions))
      (let* ((q (json-push* payload "messages" i (message (questions i))))
             (r (chat q))
             (j (string->json r))
             (a (json-ref* j "choices" 0 "message" "content")))
          (display* "Q: " (questions i) "\n")
          (display* "A: " a "\n")
          (newline)
          (loop (+ i 1)
                (json-set* q "messages" i "content" (lambda (y) (json-escape-string (string-append y "\n回答：" a))))
                (+ tokens (json-ref* j "usage" "total_tokens"))))
      (display* "Total tokens: " tokens "\n")))

