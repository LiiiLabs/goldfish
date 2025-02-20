(define (prime? n)
  (define (iter i)
    (cond ((> (* i i) n) #t)
          ((zero? (modulo n i)) #f)
          (else (iter (+ i 2)))))

  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (iter 3))))

(define (prime? n)
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else
         ((rich-list :range 3 (+ ($ n :sqrt) 1) 2)
          :forall
          (lambda (i) (not (zero? (modulo n i))))))))

(($ 1 :to 100)
 :filter prime?
 :collect)
