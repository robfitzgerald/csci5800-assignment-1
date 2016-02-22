#lang racket

;;; symbolic-diff.rkt
;;; rob fitzgerald
;;; UCD 2016sp - csci5800 - d. williams

;;; implementation of symbolic differentiation in racket
;;; ƒ (deriv exp var) and helper ƒunctions taken from 
;;; "Structure and Interpretation of Computer Programs" 
;;; by Abelson and Sussman.
;;; @see https://mitpress.mit.edu/sicp/full-text/sicp/book/node39.html

;;; (variable? e) => boolean?
;;;   e: any/c
;;; Is e a variable?
(define (variable? e)
  (symbol? e))

;;; (same-variable? v1 v2) => boolean?
;;;   v1: any/c
;;;   v2: any/c
;;; Are v1 and v2 variables?  are they the same variable?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;;; (sum? e) => boolean?
;;;   e: any/c
;;; Is e a sum?  checks the first item in the list for the plus symbol.
(define (sum? e)                  
  (and (pair? e)
       (eq? (first e) '+)))

;;; (=number? exp num) => boolean?
;;;   exp: any/c
;;;   num: number?
;;; is the value of exp equal to num?
(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

;;; (addend e) => any
;;;   e: list?
;;; Addend of the sum e, is the second item in the list.
(define (addend e)	          
  (second e))

;;; (augend e) => any
;;;   e: list?
;;; Augend of the sum e, is the third item in the list.
(define (augend e)
  (third e))

;;; (make-sum a1 a2) => list?
;;;   a1: any/c
;;;   a2: any/c
;;; Construct the sum of a1 and a2.
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

;;; (product? e) => boolean?
;;;   e: list?
;;; Is e a product? check if the first list element is the symbol for multiplication
(define (product? e)     
  (and (pair? e)
       (eq? (first e) '*)))

;;; (multiplier e) => any
;;;   e: list?
;;; Multiplier of the product e.
(define (multiplier e)	          
  (second e))

;;; (multiplicand e) => any
;;;   e: list?
;;; Multiplicand of the product e.
(define (multiplicand e)
  (third e))

;;; (make-product m1 m2) => list?
;;;   m1: any/c
;;;   m2: any/c
;;; Construct the product of m1 and m2.
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) 
              (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;;; (exponentiation? e) => boolean?
;;;   e: any/c
;;; is the provided expression an exponential?
(define (exponentiation? e)
  (and (pair? e)
       (eq? (first e) '**)))

;;; (base e) => number?
;;;   e: list?
;;; returns the base of this exponential expression
(define (base e)
  (second e))

;;; (exponent e) => number?
;;;   e: list?
;;; returns the exponent of this exponential expression
(define (exponent e)
  (third e))

;;; (make-exponentiation b e) => list?
;;;   b: any/c
;;;   e: any/c
;;; construct an exponential expression
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b)
             (number? e))
         (exponentiate b e))
        (else (list '** b e))))

;;; (exponentiate b e) => number?
;;;   b: number?
;;;   e: number?
;;; calculate an exponent
(define (exponentiate b e)
  (cond ((< e 0)
         (error "cannot exponentiate: " b e))
        ((eq? e 0) 1)
        ((> e 0)
         (* b (exponentiate b (- e 1))))))

          
;;; (deriv exp var) => list?
;;;   exp: list?
;;;   var: variable?
;;; differentiate the expression exp with respect to the variable var
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
         (make-exponentiation
           (make-product (base exp)
                         (exponent exp))
           (- (exponent exp) 1)))
        (else
         (error "unknown expression type - DERIV" exp))))