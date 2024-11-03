(define-library (srfi srfi-26)
  (import (scheme base))
  (export cut)
(begin

(define-macro (cut proc . formal-args)
  (let* ((cnt-holes 0)
          (slotted-args (map (lambda (x) (if (eq? '<> x) (begin (set! cnt-holes (+ 1)) 'slot) x)) formal-args)))
    `(lambda args
      (when (not (= (length args) ,cnt-holes)) (error 'wrong-number-of-args "The number of args should be same with the holes."))
      (let ((args-input args))
        (apply ,proc
          (map (lambda (x) (if (eq? x 'slot) 
                               (let1 subst-arg (car args-input)
                                  (set! args-input (cdr args-input))
                                  subst-arg) 
                               x)) 
              ',slotted-args)
        )))))


); end of begin
); end of define-library