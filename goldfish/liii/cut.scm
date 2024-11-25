(define-library (liii cut)
(export cut cute)
(import (liii list)
        (liii error))
(begin
(define-macro (cut . paras)
  (letrec*
    ((slot? (lambda (x) (equal? '<> x)))
     (more-slot? (lambda (x) (equal? '<...> x)))
     (slots (filter slot? paras))
     (more-slots (filter more-slot? paras))
     (xs (map (lambda (x) (gensym)) slots))
     (rest (gensym))
     (parse
       (lambda (xs paras)
         (cond
           ((null? paras) paras)
           ((not (list? paras)) paras)
           ((more-slot? (car paras)) `(,rest ,@(parse xs (cdr paras))))
           ((slot? (car paras)) `(,(car xs) ,@(parse (cdr xs) (cdr paras))))
           (else `(,(car paras) ,@(parse xs (cdr paras))))))))
    (cond
      ((null? more-slots)
       `(lambda ,xs ,(parse xs paras)))
      (else
        (when
          (or (> (length more-slots) 1)
              (not (more-slot? (last paras))))
          (error 'syntax-error "<...> must be the last parameter of cut"))
        (let ((parsed (parse xs paras)))
          `(lambda (,@xs . ,rest) (apply ,@parsed)))))))
(define-macro (cute . paras)
  (error 'not-implemented))
))
