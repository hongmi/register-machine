#! /usr/bin/guile -s
!#

(load "stack.ss")

;;make-register
(define (make-register reg-name)
  (let ((contents '*unassigned*)
	(name reg-name)
	(traced #f))
    (define (trace-on)
      (set! traced #t))
    (define (trace-off)
      (set! traced #f))
    (define (dispatch message)
      (cond ((eq? message 'name) name)
	    ((eq? message 'trace-on) trace-on)
	    ((eq? message 'trace-off) trace-off)
	    ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value)
	       (begin
		 (if traced
		     (begin
		       (display ";")
		       (display name)
		       (display ": ")
		       (display contents)
		       (display " -> ")
		       (display value)
		       (newline)))
		 (set! contents value))))
	    (else
	     (error "Unknown request - REGISTER" message))))
    dispatch))
(define (get-register-name register)
  (register 'name))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;;label
(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label - ASSEMBLE" label-name))))

;;instruction
(define (make-instruction text label)
  (cons (cons label text) '()))
(define (instruction-text inst)
  (cdar inst))
(define (instruction-label inst)
  (caar inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((eq? x (car set)) #t)
	(else
	 (element-of-set? x (cdr set)))))

;;make-new-machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(inst-count 0)
	(trace-switch #f)
	(current-label '()))
    (let ((the-ops
            (list (list 'initialize-stack
			(lambda () (stack 'initialize)))
		  (list 'print-stack-statistics
			(lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
              ;allocate by first seen
	      ;(begin
	      ;(allocate-register name)
	      ;(lookup-register name)))))
      (define (get-inst-count)
	inst-count)
      (define (reset-inst-count)
	(set! inst-count 0))
      (define (trace-on) (set! trace-switch #t))
      (define (trace-off) (set! trace-switch #f))
      (define (trace instruction)
	(if trace-switch
	    (begin 
	      (if (symbol? (instruction-label instruction))
		  (begin
		    (set! current-label (instruction-label instruction))
		    (display current-label)
		    (newline)))
	      (begin 
		(display inst-count)
		(display ": ")
		(display (instruction-text instruction))
		(newline)))))
		  
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(set! inst-count (+ inst-count 1))
		(trace (car insts))
		((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
	       (reset-inst-count)
               (execute))
              ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)    
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
	      ((eq? message 'get-inst-count) inst-count)
	      ((eq? message 'reset-inst-count) reset-inst-count)
	      ((eq? message 'trace-on) trace-on)
	      ((eq? message 'trace-off) trace-off)
              (else (error "Unknown request - MACHINE" message))))
      dispatch)))

;;start-machine
(define (start machine)
  (machine 'start))
(define (get-register machine register-name)
  ((machine 'get-register) register-name))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (trace-register-on machine register-name)
  (((get-register machine register-name) 'trace-on)))
(define (trace-register-off machine register-name)
  (((get-register machine register-name) 'trace-off)))
