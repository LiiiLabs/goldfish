;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii lang)
(import (liii base) (liii string) (liii vector) (liii sort)
        (liii list) (liii hash-table) (liii bitwise))
(export
  @
  define-case-class case-class? == != chained-define display* object->string
  option none
  rich-integer rich-char rich-string
  rich-list range stack
  rich-vector array rich-hash-table array-buffer
  box $
)
(begin

(define-macro (@ . paras)
  (letrec*
    (
      (slot? (lambda (x) (equal? '_ x)))
      (exprs (filter (lambda (x) (not (slot? x))) paras))
      (slots (filter slot? paras))

      (exprs-sym-list (map (lambda (x) (gensym)) exprs))  
      (slots-sym-list (map (lambda (x) (gensym)) slots))

      (lets (map list exprs-sym-list exprs))

      (parse
        (lambda (exprs-sym-list slots-sym-list paras)
          (cond
            ((null? paras) paras)
            ((not (list? paras)) paras)
            ((slot? (car paras)) 
              `(,(car slots-sym-list) 
                ,@(parse exprs-sym-list (cdr slots-sym-list) (cdr paras))))
            (else 
              `(,(car exprs-sym-list) 
                ,@(parse (cdr exprs-sym-list) slots-sym-list (cdr paras))))))))
                
  `(let ,lets 
        (lambda ,slots-sym-list 
                ,(parse exprs-sym-list slots-sym-list paras)))))


(define-macro (define-case-class class-name fields . methods)
  (let* ((key-fields
         (map (lambda (field) (string->symbol (string-append ":" (symbol->string (car field)))))
              fields))
         (instance-methods
          (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "%"))
                  methods))
         (instance-method-symbols (map caadr instance-methods))
         (instance-messages
          (map (lambda (method)
                 (let1 name (string-remove-prefix (symbol->string method) "%")
                   (string->symbol (string-append ":" name))))
               instance-method-symbols))
         (static-methods
          (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "@"))
                  methods))
         (static-method-symbols (map caadr static-methods))
         (static-messages
          (map (lambda (method)
                 (let1 name (string-remove-prefix (symbol->string method) "@")
                   (string->symbol (string-append ":" name))))
               static-method-symbols))
         (internal-methods
           (filter (lambda (method) (not (or (string-starts? (symbol->string (caadr method)) "%")
                                             (string-starts? (symbol->string (caadr method)) "@"))))
                   methods))
         (this-symbol (gensym)))

`(define (,class-name msg . args)

,@static-methods

(define (is-normal-function? msg)
  (and  (symbol? msg) 
        (char=? (string-ref (symbol->string msg) 0) #\:)))

(define (static-dispatcher msg . args)
    (cond
     ,@(map (lambda (method expected) `((eq? msg ,expected) (apply ,method args)))
            static-method-symbols static-messages)
     (else (value-error "No such static method " msg))))

(typed-define (create-instance ,@fields)
  (define ,this-symbol #f)
  (define (%this . xs)
    (if (null? xs)
      ,this-symbol
      (apply ,this-symbol xs)))

  (define (%is-instance-of x)
    (eq? x ',class-name))
         
  (typed-define (%equals (that case-class?))
    (and (that :is-instance-of ',class-name)
         ,@(map (lambda (field) `(equal? ,(car field) (that ',(car field))))
                fields)))
         
  (define (%apply . args)
    (cond ((null? args)
           (value-error ,class-name "Apply on zero args is not implemented"))
          ((equal? ((symbol->string (car args)) 0) #\:)
           (value-error ,class-name "No such method: " (car args)))
          (else (value-error ,class-name "No such field: " (car args)))))
         
  (define (%to-string)
    (let ((field-strings
           (list ,@(map (lambda (field key-field)
                          `(string-append
                            ,(symbol->string key-field) " "
                            (object->string ,(car field))))
                        fields key-fields))))
      (let loop ((strings field-strings)
                 (acc ""))
        (if (null? strings)
            (string-append "(" ,(symbol->string class-name) " " acc ")")
            (loop (cdr strings)
                  (if (zero? (string-length acc))
                      (car strings)
                      (string-append acc " " (car strings))))))))

  ,@internal-methods
  ,@instance-methods
 
  (define (instance-dispatcher)
    (lambda (msg . args)
      (cond
        ((eq? msg :is-instance-of) (apply %is-instance-of args))
        ((eq? msg :equals) (apply %equals args))
        ((eq? msg :to-string) (%to-string))
        ((eq? msg :this) (apply %this args))
        ,@(map (lambda (field key-field)
            `((eq? msg ,key-field)
              (,class-name
              ,@(map (lambda (f) (if (eq? (car f) (car field)) '(car args) (car f)))
                      fields))))
          fields key-fields)
        ((is-normal-function? msg)
          (cond
              ,@(map (lambda (method expected) `((eq? msg ,expected) (apply ,method args)))
               instance-method-symbols instance-messages)
              (else (value-error ,class-name "No such method: " msg))))
        
        ,@(map (lambda (field) `((eq? msg ',(car field)) ,(car field))) fields)
        (else (apply %apply (cons msg args))))))

  (set! ,this-symbol (instance-dispatcher))
  ,this-symbol
) ; end of the internal typed define

(if (in? msg (list ,@static-messages))
    (apply static-dispatcher (cons msg args))
    (apply create-instance (cons msg args)))

) ; end of define
) ; end of let
) ; end of define-macro

(define (case-class? x)
  (and-let* ((is-proc? (procedure? x))
             (source (procedure-source x))
             (source-at-least-3? (and (list? source) (>= (length source) 3)))
             (body (source 2))
             (body-at-least-3? (and (list? body) (>= (length body) 3)))
             (is-cond? (eq? (car body) 'cond))
             (pred1 ((body 1) 0))
             (pred2 ((body 2) 0)))
    (and (equal? pred1 '(eq? msg :is-instance-of))
         (equal? pred2 '(eq? msg :equals)))))

(define (== left right)
  (cond
    ((and (case-class? left) (case-class? right))
     (left :equals right))
    ((case-class? left)
     (left :equals ($ right)))
    ((case-class? right)
     ($ left :equals right))
    (else
     (equal? left right))))

(define (!= left right)
  (not (== left right)))

(define-macro (chained-define head . body)
  (let ((xs (gensym))
        (result (gensym)))
    `(define ,(append head xs)
       (let ((,result (begin ,@body)))
         (if (null? ,xs)
           ,result
           (apply ,result ,xs))))))

(define (display* . params)
  (define (%display x)
    (if (case-class? x)
        (display (x :to-string))
        (display x)))
  (for-each %display params))

(define s7-object->string object->string)

(define (object->string x)
  (if (case-class? x)
      (x :to-string)
      (s7-object->string x)))

(define-case-class option ((value any?))

(define (%get)
  (if (null? value)
      (value-error "option is empty, cannot get value")
      value))

(define (%get-or-else default)
  (cond ((not (null? value)) value)
        ((and (procedure? default) (not (case-class? default)))
         (default))
        (else default)))

(typed-define (%or-else (default case-class?))
  (when (not (default :is-instance-of 'option))
    (type-error "The first parameter of option%or-else must be a option case class"))

  (if (null? value)
      default
      (option value)))

(define (%equals that)
  (== value (that 'value)))

(define (%defined?) (not (null? value)))
  
(define (%empty?) (null? value))

(define (%forall f)
  (if (null? value)
      #f
      (f value)))

(define (%exists f)
  (if (null? value)
      #f
      (f value)))

(define (%for-each f)
  (when (not (null? value))
        (f value)))

(chained-define (%map f)
  (if (null? value)
      (option '())
      (option (f value))))

(define (%flat-map f . xs)
  (let1 r (if (null? value)
              (option '())
              (f value))
    (if (null? xs) r (apply r xs))))

(define (%filter pred . xs)
    (let1 r (if (or (null? value) (not (pred value)))
               (option '())
               (option value))
      (if (null? xs) r (apply r xs))))

)

(define (none) (option '()))

(define-case-class rich-integer ((data integer?))

(define (%get) data)

(typed-define (%to (n integer?))
  (if (< n data)
      (rich-list (list))
      (rich-list (iota (+ (- n data) 1) data))))

(typed-define (%until (n integer?))
  (if (<= n data)
      (rich-list (list))
      (rich-list (iota (+ (- n data)) data))))

(define (%to-char)
  (rich-char data))

(define (%to-string)
  (number->string data))

(define (@max-value) 9223372036854775807)

(define (@min-value) -9223372036854775808)

;;return exact integer
(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

)

(define-case-class rich-char ((code-point integer?))

(define (%digit?)
  (or
   (and (>= code-point 48) (<= code-point 57))
   (and (>= code-point #xFF10) (<= code-point #xFF19))
   (and (>= code-point #x0660) (<= code-point #x0669))
   (and (>= code-point #x06F0) (<= code-point #x06F9))
   (and (>= code-point #x0966) (<= code-point #x096F))
   (and (>= code-point #x09E6) (<= code-point #x09EF))
   (and (>= code-point #x0A66) (<= code-point #x0A6F))
   (and (>= code-point #x0AE6) (<= code-point #x0AEF))
   (and (>= code-point #x0B66) (<= code-point #x0B6F))
   (and (>= code-point #x0BE6) (<= code-point #x0BEF))
   (and (>= code-point #x0C66) (<= code-point #x0C6F))
   (and (>= code-point #x0CE6) (<= code-point #x0CEF))
   (and (>= code-point #x0D66) (<= code-point #x0D6F))
   (and (>= code-point #x0E50) (<= code-point #x0E59))
   (and (>= code-point #x0ED0) (<= code-point #x0ED9))
   (and (>= code-point #x0F20) (<= code-point #x0F29))
   (and (>= code-point #x1040) (<= code-point #x1049))
   (and (>= code-point #x17E0) (<= code-point #x17E9))
   (and (>= code-point #x1810) (<= code-point #x1819))))
  
(define (%to-upper . xs)
  (let1 r (rich-char
    (if (and (>= code-point #x61) (<= code-point #x7A))
      (bitwise-and code-point #b11011111)
      code-point))
    (if (null? xs) r (apply r xs))))

(define (%to-lower . xs)
  (let1 r (rich-char
    (if (and (>= code-point #x41) (<= code-point #x5A))
      (bitwise-ior code-point #b00100000)
      code-point))
    (if (null? xs) r (apply r xs))))

(define (%to-bytevector)
  (cond
    ((<= code-point #x7F)
     (bytevector code-point))

    ((<= code-point #x7FF)
     (let ((byte1 (bitwise-ior #b11000000 (bitwise-and (arithmetic-shift code-point -6) #b00011111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2)))

    ((<= code-point #xFFFF)
     (let ((byte1 (bitwise-ior #b11100000 (bitwise-and (arithmetic-shift code-point -12) #b00001111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
           (byte3 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2 byte3)))

    ((<= code-point #x10FFFF)
     (let ((byte1 (bitwise-ior #b11110000 (bitwise-and (arithmetic-shift code-point -18) #b00000111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -12) #b00111111)))
           (byte3 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
           (byte4 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2 byte3 byte4)))

    (else
     (value-error "Invalid code point"))))

(define (%to-string)
  (utf8->string (%to-bytevector)))

)

(define make-rich-char rich-char)

(define (utf8-byte-sequence->code-point byte-seq)
  (let ((len (bytevector-length byte-seq)))
    (cond
      ((= len 1)
       (bytevector-u8-ref byte-seq 0))
      ((= len 2)
       (let ((b1 (bytevector-u8-ref byte-seq 0))
             (b2 (bytevector-u8-ref byte-seq 1)))
         (bitwise-ior
          (arithmetic-shift (bitwise-and b1 #x1F) 6)
          (bitwise-and b2 #x3F))))
      ((= len 3)
       (let ((b1 (bytevector-u8-ref byte-seq 0))
             (b2 (bytevector-u8-ref byte-seq 1))
             (b3 (bytevector-u8-ref byte-seq 2)))
         (bitwise-ior
          (arithmetic-shift (bitwise-and b1 #x0F) 12)
          (arithmetic-shift (bitwise-and b2 #x3F) 6)
          (bitwise-and b3 #x3F))))
      ((= len 4)
       (let ((b1 (bytevector-u8-ref byte-seq 0))
             (b2 (bytevector-u8-ref byte-seq 1))
             (b3 (bytevector-u8-ref byte-seq 2))
             (b4 (bytevector-u8-ref byte-seq 3)))
         (bitwise-ior
          (arithmetic-shift (bitwise-and b1 #x07) 18)
          (arithmetic-shift (bitwise-and b2 #x3F) 12)
          (arithmetic-shift (bitwise-and b3 #x3F) 6)
          (bitwise-and b4 #x3F))))
      (else
       (value-error "Invalid UTF-8 byte sequence length")))))

(define (rich-char x)
  (cond ((integer? x)
         (if (and (>= x 0) (<= x #x10FFFF))
             (make-rich-char x)
             (value-error "rich-char: code point out of range" x)))
        ((string? x)
         (if (= 1 (u8-string-length x))
             (rich-char (string->utf8 x))
             (value-error "rich-char: must be u8 string which length equals 1")))
        ((bytevector? x)
         (make-rich-char (utf8-byte-sequence->code-point x)))
        (else (type-error "rich-char: must be integer, string, bytevector"))))

(define-case-class rich-string ((data string?))

(define (%get) data)

(define (%length)
  (u8-string-length data))

(define (%char-at index)
  (let* ((start index)
         (end (+ index 1))
         (byte-seq (string->utf8 data start end))
         (code-point (utf8-byte-sequence->code-point byte-seq)))
    (rich-char byte-seq)))

(typed-define (%apply (i integer?))
  (%char-at i))

(chained-define (%slice from until)
  (let* ((len (u8-string-length data))
         (start (max 0 from))
         (end (min len until)))
    (if (< start end)
        (rich-string (u8-substring data start end))
        (rich-string ""))))

(define (%empty?)
  (string-null? data))

(define (%starts-with prefix)
  (string-starts? data prefix))

(define (%ends-with suffix)
  (string-ends? data suffix))

(define (%forall pred)
  ((%to-vector) :forall pred))

(define (%exists pred)
  ((%to-vector) :exists pred))

(define (%contains elem)
  (cond ((string? elem)
         (string-contains data elem))
        ((char? elem)
         (string-contains data (string elem)))
        (else (type-error "elem must be char or string"))))

;; Find the index for the char or substring in rich-string (from start-index), else return -1
(define (%index-of sub . start-index)
  (let1 start (if (null? start-index) 0 (car start-index))
  (cond
    ((string? sub)
     (let ((str-len (string-length data))
           (sub-len (string-length sub)))
       (let loop ((i start))
         (cond
           ((> (+ i sub-len) str-len) -1)
           ((equal? (substring data i (+ i sub-len)) sub) i)
           (else (loop (+ i 1)))))))
    ((char? sub)
     (let loop ((lst (string->list (substring data start))) (index start))
       (cond
       ((null? lst) -1)
       ((char=? (car lst) sub) index)
       (else (loop (cdr lst) (+ index 1)))))))))

(chained-define (%map f)
  (rich-string
    (%to-vector :map f
                :map (@ _ :to-string)
                :make-string)))

(define (%count pred?)
  ((%to-vector) :count pred?))

(define (%to-string)
  data)

(chained-define (%to-vector)
  (if (string-null? data)
      (rich-vector :empty)
      (let* ((bv (string->utf8 data))
             (bv-size (length bv))
             (len (u8-string-length data))
             (result (make-vector len)))
        (let loop ((i 0) (j 0))
             (if (>= i len)
                 (rich-vector result)
                 (let* ((next-j (bytevector-advance-u8 bv j bv-size))
                        (code (utf8-byte-sequence->code-point (bytevector-copy bv j next-j))))
                   (vector-set! result i (rich-char code))
                   (loop (+ i 1) next-j)))))))

(define (%strip-prefix prefix . xs)
  (let ((result (rich-string (string-remove-prefix data prefix))))
    (if (null? xs)                                 
        result
        (apply result xs)))) 

(define (%strip-suffix suffix . xs)
  (let ((result (rich-string (string-remove-suffix data suffix))))
    (if (null? xs)                                 
        result
        (apply result xs)))) 
;; Replace the first occurrence of the substring old to new.
(define (%replace-first old new . xs)
  (define (replace-helper str old new start)
    (let ((next-pos (%index-of old start)))
      (if (= next-pos -1)
          str
          (string-append
           (substring str 0 next-pos)
           new
           (substring str (+ next-pos (string-length old)))))))
  (let ((result (rich-string (replace-helper data old new 0))))
    (if (null? xs)
        result
        (apply result xs))))

;; Replace the occurrences of the substring old to new.
(define (%replace old new . xs)
  (define (replace-helper str old new start)
    (let ((next-pos ((rich-string str) :index-of old start)))
      (if (= next-pos -1)
          str
          (replace-helper ((rich-string str) :replace-first old new :get) old new next-pos))))
  (let ((result (rich-string (replace-helper data old new 0))))
    (if (null? xs)
        result
        (apply result xs))))

;; Split string with sep.
(define (%split sep . xs)
  (let ((str-len (string-length data))
        (sep-len (string-length sep)))
    ; tail recursive auxiliary function
    (define (split-helper start acc)
      (let ((next-pos (%index-of sep start)))
        (if (= next-pos -1)
            (cons (substring data start) acc)
            (split-helper (+ next-pos sep-len) (cons (substring data start next-pos) acc)))))
    ; do split
    (let1 r (rich-vector
      (if (zero? sep-len)
          ((%to-vector) :map (lambda (c) (c :to-string)) :collect)
          (list->vector (reverse (split-helper 0 '())))))
          (if (null? xs) r (apply r xs)))))

)

(define-case-class rich-list ((data list?))

(define (@range start end . step)
  (let ((step-size (if (null? step) 1 (car step))))
    (cond
      ((and (positive? step-size) (>= start end))
       (rich-list '()))
      ((and (negative? step-size) (<= start end))
       (rich-list '()))
      ((zero? step-size)
       (value-error "Step size cannot be zero"))
      (else
       (let1 cnt (ceiling (/ (- end start) step-size))
         (rich-list (iota cnt start step-size)))))))

(define (@empty . xs)
  (let1 r (rich-list (list ))
    (if (null? xs) r (apply r xs))))

(define (@concat lst1 lst2 . xs)
  (let1 r (rich-list (append (lst1 :collect) (lst2 :collect)))
    (if (null? xs) r (apply r xs))))

(define (@fill n elem)
  (cond
    ((< n 0)
      (value-error "n cannot be negative"))
    ((= n 0)
      (rich-list '()))
    (else
      (rich-list (make-list n elem)))))

(define (%collect) data)

(define (%apply n)
  (list-ref data n))

(define (%find pred)
  (let loop ((lst data))
    (cond
      ((null? lst) (none))
      ((pred (car lst)) (option (car lst)))
      (else (loop (cdr lst))))))

(define (%head)
  (if (null? data)
      (error 'out-of-range "rich-list%head: list is empty")
      (car data)))


(define (%head-option)
  (if (null? data)
      (none)
      (option (car data))))


(define (%empty?)
  (null? data))

(define (%equals that)
  (let* ((l1 data)
         (l2 (that 'data))
         (len1 (length l1))
         (len2 (length l2)))
    (if (not (eq? len1 len2))
        #f
        (let loop ((left l1) (right l2))
          (cond ((null? left) #t)
                ((!= (car left) (car right)) #f)
                (else (loop (cdr left) (cdr right))))))))

(define (%forall pred)
  (every pred data))

(define (%exists pred)
  (any pred data))

(define (%contains elem)
  (%exists (lambda (x) (equal? x elem))))

  (define (%map x . xs)
    (let1 r (rich-list (map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%flat-map x . xs)
    (let1 r (rich-list (flat-map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%filter x . xs)
    (let1 r (rich-list (filter x data))
      (if (null? xs) r (apply r xs))))

  (define (%for-each x)
    (for-each x data))

  (define (%take x . xs)
    (typed-define (scala-take (data list?) (n integer?))
      (cond ((< n 0) '())
            ((>= n (length data)) data)
            (else (take data n))))

    (let1 r (rich-list (scala-take data x))
      (if (null? xs) r (apply r xs))))

(define (%drop x . xs)
    (typed-define (scala-drop (data list?) (n integer?))
      (cond ((< n 0) data)
            ((>= n (length data)) '())
            (else (drop data n))))

    (let1 r (rich-list (scala-drop data x))
      (if (null? xs) r (apply r xs))))

  (define (%take-right x . xs)
    (typed-define (scala-take-right (data list?) (n integer?))
      (cond ((< n 0) '())
            ((>= n (length data)) data)
            (else (take-right data n))))

    (let1 r (rich-list (scala-take-right data x))
      (if (null? xs) r (apply r xs))))

 (define (%drop-right x . xs)
    (typed-define (scala-drop-right (data list?) (n integer?))
      (cond ((< n 0) data)
            ((>= n (length data)) '())
            (else (drop-right data n))))

    (let1 r (rich-list (scala-drop-right data x))
      (if (null? xs) r (apply r xs))))
 
  (define (%count . xs)
    (cond ((null? xs) (length data))
          ((length=? 1 xs) (count (car xs) data))
          (else (error 'wrong-number-of-args "rich-list%count" xs))))

  (define (%fold initial f)
    (fold f initial data))

  (define (%fold-right initial f)
    (fold-right f initial data))

(define (%sort-with less-p . xs)
  (let* 
    ((sorted-data (list-stable-sort less-p data))
    (sorted-rich-list (rich-list sorted-data)))
    (if (null? xs) 
        sorted-rich-list 
        (apply sorted-rich-list xs))))

(define (%to-string)
  (object->string data))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let1 sep (car xs)
         (if (string? sep)
             (values "" sep "")
             (type-error "rich-list%make-string: separator must be a string" sep))))
      ((length=? 2 xs)
       (error 'wrong-number-of-args "rich-list%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
      ((length=? 3 xs)
       (let ((start (car xs))
             (sep (cadr xs))
             (end (caddr xs)))
         (if (and (string? start) (string? sep) (string? end))
             (values start sep end)
             (error 'type-error "rich-list%make-string: prefix, separator, and suffix must be strings" xs))))
      (else (error 'wrong-number-of-args "rich-list%make-string: expected 0, 1, or 3 arguments" xs))))

  (receive (start sep end) (parse-args xs)
    (let1 as-string (lambda (x) (if (string? x) x (object->string x)))
          (string-append start (string-join (map as-string data) sep) end))))

)

(define-case-class array-buffer ((data vector?) (size integer?) (capacity integer?))

(chained-define (@from-vector vec)
  (let ((len (vector-length vec)))
    (array-buffer (copy vec) len len)))

(chained-define (@from-list lst)
  (let ((len (length lst)))
    (array-buffer (copy lst (make-vector len)) len len)))

(define (check-bound n)
  (when (or (not (integer? n)) (< n 0) (>= n size))
    (key-error "array-buffer out of bound")))

(define (%collect) (copy data (make-vector size)))

(define (%length) size)

(chained-define (%apply n)
  (check-bound n)
  (vector-ref data n))

(chained-define (%set! n v)
  (check-bound n)
  (vector-set! data n v))

(define (%update! . args)
  (apply %set! args))

(chained-define (%extend! n)
  (when (< capacity n)
    (if (= capacity 0)
      (set! capacity n)
      (let loop ()
        (when (< capacity n)
          (set! capacity (* 2 capacity))
          (loop))))
    (set! data (copy data (make-vector capacity) 0 size)))
  (%this))

(define (%size-hint! . args) (apply %extend! args))

(chained-define (%resize! n)
  (%extend! n)
  (set! size n)
  (%this))

(chained-define (%add-one! x)
  (%extend! (+ size 1))
  (vector-set! data size x)
  (set! size (+ size 1))
  (%this))

(chained-define (%clear!)
  (set! size 0)
  (%this))

(chained-define (%clear/shrink!)
  (set! size 0)
  (set! capacity 1)
  (set! data (make-vector 1))
  (%this))

(chained-define (%insert! index elem)
  (%extend! (+ size 1))
  (set! size (+ size 1))
  (check-bound index)
  (let loop ((p (- size 1)))
    (when (> p index)
      (vector-set! data p (vector-ref data (- p 1)))
      (loop (- p 1))))
  (vector-set! data index elem)
  (%this))

(typed-define (%equals (that case-class?))
  (and (that :is-instance-of 'array-buffer)
       ((%this :to-rich-vector) :equals (that :to-rich-vector))))

(chained-define (%to-rich-vector) (rich-vector (copy data (make-vector size))))

(chained-define (%to-rich-list) (rich-list (vector->list (copy data (make-vector size)))))

) ; end of array-buffer

(define-case-class range
  ((start integer?) (end integer?) (step integer?) (inclusive? boolean?))

(define* (@inclusive start end (step 1))
  (range start end step #t))

(define (%empty?)
  (or (and (> start end) (> step 0))
      (and (< start end) (< step 0))
      (and (= start end) (not inclusive?))))

)

(define-case-class stack ((data list?))

(define (%length) (length data))

(define (%size) (length data))

(define (%top)
  (if (null? data)
      (error 'out-of-range)
      (car data)))

(define (%to-list) (rich-list data))

(define (@empty) (stack (list )))

(define (%pop . xs)
  (let1 r
      (if (null? data)
          (error 'out-of-range)
          (stack (cdr data)))
    (if (null? xs)
        r
        (apply r xs))))

(define (%push element . es) 
  (let1 r (stack (cons element data))
    (if (null? es)
        r
        (apply r es))))

)

(define-case-class rich-vector ((data vector?))

(define (@range start end . step)
  (let ((step-size (if (null? step) 1 (car step))))
    (cond
      ((and (positive? step-size) (>= start end))
       (rich-vector #()))
      ((and (negative? step-size) (<= start end))
       (rich-vector #()))
      ((zero? step-size)
       (value-error "Step size cannot be zero"))
      (else
       (let1 cnt (ceiling (/ (- end start) step-size))
         (rich-vector (list->vector (iota cnt start step-size))))))))

(define (@empty . xs)
  (let1 r (rich-vector #())
    (if (null? xs) r (apply r xs))))

(define (@fill n elem . xs)
  (unless (integer? n)
    (type-error "n must be integer" n))
  (when (< n 0)
    (value-error "n must be non-negative" n))

  (let1 r (rich-vector (make-vector n elem))
        (if (null? xs) r (apply r xs))))

(define (%collect) data)

(define (%apply n)
  (vector-ref data n))

(define (%find p)
  (let loop ((i 0))
    (cond
     ((>= i (vector-length data)) (none))
     ((p (vector-ref data i)) (option (vector-ref data i)))
     (else (loop (+ i 1))))))

(define (%head)
  (if (> (vector-length data) 0)
      (vector-ref data 0)
      (error 'out-of-range "out-of-range")))

(define (%head-option)
  (if (> (vector-length data) 0)
      (option (vector-ref data 0))
      (none)))

(define (%last)
  (let ((len (vector-length data)))
    (if (> len 0)
      (vector-ref data (- len 1))
      (error 'out-of-range "out-of-range"))))

(define (%last-option)
  (let ((len (vector-length data)))
    (if (> len 0)
      (option (vector-ref data (- len 1)))
      (none))))

(define (%slice from until . xs)
  (let* ((len (vector-length data))
         (start (max 0 from))
         (end (min len until)))
    (let ((res (if (< start end)
                 (rich-vector (vector-copy data start end))
                 (rich-vector :empty))))
      (if (null? xs) res (apply res xs)))))

(define (%empty?)
  (= (length data) 0))

(define (%equals that)
  (and (that :is-instance-of 'rich-vector)
       (vector= == data (that 'data))))

(define (%forall p)
  (vector-every p data))

(define (%exists p)
  (vector-any p data))

  (define (%map x . xs)
    (let1 r (rich-vector (vector-map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%filter x . xs)
    (let1 r (rich-vector (vector-filter x data))
      (if (null? xs) r (apply r xs))))

  (define (%for-each x)
    (vector-for-each x data))

  (define (%count . xs)
    (cond ((null? xs) (vector-length data))
          ((length=? 1 xs) (vector-count (car xs) data))
          (else (error 'wrong-number-of-args "rich-vector%count" xs))))

  (define (%take x . xs)
    (typed-define (scala-take (data vector?) (n integer?))
      (cond
        ((< n 0) (vector))
        ((>= n (vector-length data)) data)
        (else
          (let ((new-vec (make-vector n)))
            (do ((i 0 (+ i 1)))
                ((>= i n) new-vec)
              (vector-set! new-vec i (vector-ref data i)))))))

    (let1 r (rich-vector (scala-take data x))
      (if (null? xs) r (apply r xs))))

  (define (%take-right x . xs)
    (typed-define (scala-take-right (data vector?) (n integer?))
      (let ((len (vector-length data)))
        (cond
          ((< n 0) (vector))
          ((>= n len) data)
          (else
            (let ((new-vec (make-vector n)))
              (do ((i (- len n) (+ i 1))
                   (j 0 (+ j 1)))
                  ((>= j n) new-vec)
                (vector-set! new-vec j (vector-ref data i))))))))

    (let1 r (rich-vector (scala-take-right data x))
      (if (null? xs) r (apply r xs))))

(define (%drop x . xs)
    (typed-define (scala-drop (data vector?) (n integer?))
      (cond
        ((< n 0) data)
        ((>= n (vector-length data)) (vector))
        (else (vector-copy data n))))
        
    (let1 r (rich-vector (scala-drop data x))
      (if (null? xs) r (apply r xs))))

(define (%drop-right x . xs)
  (typed-define (scala-drop-right (data vector?) (n integer?))
    (cond
      ((< n 0) data) 
      ((>= n (vector-length data)) (vector)) 
      (else (vector-copy data 0 (- (vector-length data) n))))) 

  (let1 r (rich-vector (scala-drop-right data x))  
    (if (null? xs) r (apply r xs))))

  (define (%fold initial f)
    (vector-fold f initial data))

  (define (%fold-right initial f)
    (vector-fold-right f initial data))

  (define (%count . xs)
    (cond ((null? xs) (vector-length data))
          ((length=? 1 xs) (count (car xs) (vector->list data)))
          (else (error 'wrong-number-of-args "rich-vector%count" xs))))

(define (%sort-with less-p . xs)
  (let ((res (rich-vector (vector-stable-sort less-p data))))
    (if (null? xs)
      res
      (apply res xs))))

(define (%to-string)
  (object->string data))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let1 sep (car xs)
         (if (string? sep)
             (values "" sep "")
             (type-error "rich-vector%make-string: separator must be a string" sep))))
      ((length=? 2 xs)
       (error 'wrong-number-of-args "rich-vector%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
      ((length=? 3 xs)
       (let ((start (car xs))
             (sep (cadr xs))
             (end (caddr xs)))
         (if (and (string? start) (string? sep) (string? end))
             (values start sep end)
             (type-error "rich-vector%make-string: prefix, separator, and suffix must be strings" xs))))
      (else (error 'wrong-number-of-args "rich-vector%make-string: expected 0, 1, or 3 arguments" xs))))

  (receive (start sep end) (parse-args xs)
    (let* ((as-string (lambda (x) (if (string? x) x (object->string x))))
           (middle (string-join (map as-string (vector->list data)) sep)))
      (string-append start middle end))))

(define (%set! i x)
  (when (or (< i 0) (>= i (length data)))
    (error 'out-of-range "rich-vector%set! out of range at index" i))
  (vector-set! data i x))

)

(define array rich-vector)

(define-case-class rich-hash-table ((data hash-table?))
  (define (%collect) data)

(define (@empty . xs)
  (let1 r (rich-hash-table (make-hash-table))
    (if (null? xs) r (apply r xs))))

(define (%find pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) (none))
        ((and (pair? kv) (pred? (car kv) (cdr kv))) (option kv))
        (else (loop (iter))))))

(define (%get k)
  (option (hash-table-ref/default data k '())))

(define (%contains k)
  (hash-table-contains? data k))

(define (%forall pred?)
  (let ((all-kv (map identity data)))
    (let loop ((kvs all-kv))  
      (if (null? kvs)
          #t  
          (let1 kv (car kvs)
            (if (pred? (car kv) (cdr kv))
                (loop (cdr kvs))  
                #f))))))  

(define (%exists pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) #f)
        ((and (pair? kv) (pred? (car kv) (cdr kv))) #t)
        (else (loop (iter))))))

(chained-define (%map f)
  (let1 r (make-hash-table)
    (hash-table-for-each
       (lambda (k v)
         (receive (k1 v1) (f k v)
           (hash-table-set! r k1 v1)))
       data)
    (rich-hash-table r)))

(define (%count pred)
  (hash-table-count pred data))

(define (%foreach proc)
  (hash-table-for-each proc data))

)

(define (box x)
  (cond ((integer? x) (rich-integer x))
        ((char? x) (rich-char (char->integer x)))
        ((string? x) (rich-string x))
        ((list? x) (rich-list x))
        ((vector? x) (rich-vector x))
        ((hash-table? x) (rich-hash-table x))
        (else (type-error "box: x must be integer?, char?, string?, list?, vector?, hash-table?"))))

(define ($ x . xs)
  (if (null? xs) (box x) (apply (box x) xs)))

) ; end of begin
) ; end of library

