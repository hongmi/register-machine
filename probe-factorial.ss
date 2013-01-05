#! /usr/bin/guile -s
!#

(load "machine.ss")

(define fact-machine 
  (make-machine
   '(continue n val)
   (list (list '- -) (list '= =) (list '* *))
   '(controller
     (assign continue (label fact-done))
   fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up so that the computation will continue
     ;; at after-fact when the subroutine returns
     (save continue)
     (save n)     
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
   after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ; val now contains n(n-1)!
     (goto (reg continue))                 ; return to caller
   base-case
     (assign val (const 1))                ; base case: 1! = 1
     (goto (reg continue))                 ; return to caller
     fact-done)))


(define (stat n)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (display (get-register-contents fact-machine 'val))
  ((fact-machine 'stack) 'print-statistics)
  (newline))

(define (make-list n)
  (if (= n 0)
      '()
      (append (make-list (- n 1)) (list n))))
(define (stat-total n)
  (map stat (make-list n)))

(display "input a number: ")
(stat-total (read))