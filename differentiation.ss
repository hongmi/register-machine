
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum-two a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (single-expression? x) (null? (cdr x)))
(define (make-sum a1 . a2)
  (cond ((single-expression? a2) (make-sum-two a1 (car a2)))
	(else
	 (cons '+ (cons a1 a2)))))

(define (make-product-two m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	(else (list '* m1 m2))))

(define (make-product m1 . m2)
  (if (single-expression? m2)
      (make-product-two m1 (car m2))
      (cons '* (cons m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (single-expression? (cddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (single-expression? (cddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (** base exp)
  (cond ((base 0) 0)
	((exp 0) 1)
	(else
	 (* exp (** base (- exp 1))))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation e1 e2) (list '** e1 e2))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation (base exp) (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type - DERIV" exp))))