#! /usr/bin/guile -s
!#

(load "machine.ss")

(define (count-leaves tree)
 (cond ((null? tree) 0)
       ((not (pair? tree)) 1)
       (else (+ (count-leaves (car tree))
		(count-leaves (cdr tree))))))

(define count-leaves-machine
  (make-machine
   '(continue count tree n)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
	  (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label done))
     (assign count (const 0))
   tree-loop
     (test (op null?) (reg tree))
     (branch (label null-case))
     (test (op pair?) (reg tree))
     (branch (label left-tree))
     (assign count (const 1))
     (goto (reg continue))
   left-tree
     (save continue)
     (save tree)
     (assign continue (label right-tree))
     (assign tree (op car) (reg tree))
     (goto (label tree-loop))
   right-tree
     (restore tree)
     (restore continue)
     (save continue)
     (save count)
     (assign continue (label add-tree))
     (assign tree (op cdr) (reg tree))
     (goto (label tree-loop))
   add-tree
     (assign n (reg count))
     (restore count)
     (restore continue)
     (assign count (op +) (reg count) (reg n))
     (goto (reg continue))
   null-case
     (assign count (const 0))
     (goto (reg continue))
     done)))

(set-register-contents! count-leaves-machine 'tree '(a (b c (d (2 3) d)) (e f) g))
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'count)
