#! /usr/bin/guile -s
!#

(load "machine.ss")

(define expt-machine
  (make-machine
    '(n b val continue)
    (list (list '* *) (list '- -) (list '= =))
    '(controller
      (assign continue (label expt-done))  ;;set up final return addr
    expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      ;;save continue
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
    after-expt
      (restore continue)
      (assign val (op *) (reg val) (reg b))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    expt-done)))

(display "base: ")
(set-register-contents! expt-machine 'b (read))
(display "exponent: ")
(set-register-contents! expt-machine 'n (read))
(start expt-machine)
(display (get-register-contents expt-machine 'val))
(newline)
