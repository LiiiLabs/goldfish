(import (liii stack)
        (liii check))

(check-set-mode! 'report-failed)

(define stack1 (stack))
(check (stack-empty? stack1) => #t)
(check (stack->list stack1) => '())
(check-catch 'value-error (stack-pop! stack1))

(stack-push! stack1 1)
(check (stack->list stack1) => '(1))
(check (stack-top stack1) => 1)
(check (stack-size stack1) => 1)
(check (stack-pop! stack1) => 1)
(check (stack-empty? stack1) => #t)
(check (stack-size stack1) => 0)

(define stack2 (stack 1 2 3))
(check (stack->list stack2) => '(1 2 3))
(check (stack-size stack2) => 3)
(check (stack-pop! stack2) => 1)
(check (stack-pop! stack2) => 2)
(check (stack-pop! stack2) => 3)

(define stack3 (stack ))
(stack-push! stack3 1)
(stack-push! stack3 2)
(stack-push! stack3 3)
(check (stack-pop! stack3) => 3)
(check (stack-pop! stack3) => 2)
(check (stack-pop! stack3) => 1)

(check-catch 'type-error (stack-empty? 1))

