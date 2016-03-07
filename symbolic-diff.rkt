#lang racket

;;; symbolic-diff.rkt
;;; rob fitzgerald
;;; UCD 2016sp - csci5800 - d. williams
;;; implementation of symbolic differentiation in racket
;;; ƒ (deriv exp var) and helper ƒunctions taken from 
;;; "Structure and Interpretation of Computer Programs" 
;;; by Abelson and Sussman.
;;; @see https://mitpress.mit.edu/sicp/full-text/sicp/book/node39.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; developer-extensible operations: differentiation and simplification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; see examples below for differentiation and simplification of        ;;;
;;; sums, products, and exponents.                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; diff-table => hash?
;;; table of differentiation functions
(define diff-table (make-hash))

;;; (define-differentiator (op exp var) (body ...))
;;;   op: arithmetic operator
;;;   exp: list?
;;;   var: variable?
;;;   body: a function that takes two parameters, 'exp' and 'var'
;;; interface for developer to define new differentiation functions
(define-syntax-rule (define-differentiator (op exp var)
                      (body ...))
  (hash-set! diff-table 'op
             (lambda (exp var)
               body ...)))

;;; ((hash-ref diff-table +) exp var) => list?
;;;   exp: list?
;;; Calculates the derivative of a sum.
(define-differentiator (+ exp var)
  (simplify (list '+ (deriv (addend exp) var)
            (deriv (augend exp) var))))

;;; ((hash-ref diff-table *) exp var) => list?
;;;   exp: list?
;;; Calculates the derivative of a product.
(define-differentiator (* exp var)
  (simplify (list '+
   (simplify (list '* (multiplier exp)
             (deriv (multiplicand exp) var)))
   (simplify (list '* (deriv (multiplier exp) var)
             (multiplicand exp))))))

;;; ((hash-ref diff-table **) exp var) => list?
;;;   exp: list?
;;; Calculates the derivative of an exponent.
(define-differentiator (** exp var)
  (simplify (list '**
   (simplify (list '* (base exp)
             (exponent exp)))
   (- (exponent exp) 1))))


;;; simplification-table => hash?
;;; table of simplification functions
;;; (replaces 'make-sum', 'make-product', 'make-exponentiation')
(define simplification-table (make-hash))

;;; (simplify exp) => list?
;;;   exp: an expression / list to be simplified
;;; apply simplification rules stored in simplification-table to an expression
(define (simplify exp)
  (cond ((and (pair? exp)
               (symbol? (car exp))
               (hash-has-key? simplification-table (car exp)))
          ((hash-ref simplification-table (car exp)) exp))
         (else
          exp)))

;;; (define-simplifier (op exp) (body ...))
;;;   op: arithmetic operator
;;;   exp: list?
;;;   body: a function that takes one parameter, 'exp'
;;; interface for developer to define new differentiation functions
(define-syntax-rule (define-simplifier (op exp)
                      (body ...))
  (hash-set! simplification-table 'op
             (lambda (exp)
               body ...)))

;;; (simplify-recurse op exp pos) -> number? | list?
;;;   op: symbol?
;;;   exp: list?
;;;   pos: number?
;;; applies simplification to expressions that have more than 2 operands
;;; used by all operation types.
;;; runs through remainder of list with overlapping binary simplifications.
(define (simplify-recurse op exp pos)
  (cond ((>= (+ pos 2) (length exp))
         (simplify (list op (list-ref exp pos) (list-ref exp (+ pos 1)))))
        (else
         (simplify (list op (list-ref exp pos) (simplify-recurse op exp (+ pos 1)))))))

;;; ((hash-ref simplification-table +) exp) => list? | number?
;;;   exp: list?
;;; Simplifies a sum expression.
(define-simplifier (+ exp)
  ((cond ((=number? (addend exp) 0) 
         (augend exp))
        ((=number? (augend exp) 0) 
         (addend exp))
        ((and (number? (addend exp))
               (number? (augend exp)))
         (+ (addend exp) (augend exp)))
        (else exp))))

;;; ((hash-ref simplification-table *) exp) => list? | number?
;;;   exp: list?
;;; Simplifies a product expression.
(define-simplifier (* exp)
  ((cond ((or (=number? (multiplier exp) 0) (=number? (multiplicand exp) 0)) 0)
        ((=number? (multiplier exp) 1) (multiplicand exp))
        ((=number? (multiplicand exp) 1) (multiplier exp))
        ((and (number? (multiplier exp)) 
              (number? (multiplicand exp)))
         (* (multiplier exp) (multiplicand exp)))
        (else exp))))

;;; ((hash-ref simplification-table **) exp) => list? | number?
;;;   exp: list?
;;; Simplifies an exponential expression
(define-simplifier (** exp)
  ((cond ((=number? (exponent exp) 0) 1)
        ((=number? (exponent exp) 1) (base exp))
        ((and (number? (base exp))
             (number? (exponent exp)))
         (exponentiate (base exp) (exponent exp)))
        (else (list '** (base exp) (exponent exp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deriv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example
;;; > (deriv '(* (** x 4) 2) 'x)
;;; '(* (** (* x 4) 3) 2)         ; d/dx(2 * (x^4) dx) = 8x^3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (deriv exp var) => list?
;;;   exp: list?
;;;   var: variable?
;;; differentiate the expression exp with respect to the variable var
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((and (pair? exp)
              (symbol? (car exp)))
         (simplify ((hash-ref diff-table (car exp)) exp var)))
        (else
         (error "unknown expression type - DERIV" exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; (=number? exp num) => boolean?
;;;   exp: any/c
;;;   num: number?
;;; is the value of exp equal to num?
(define (=number? exp num)
  (and (number? exp)
       (= exp num)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sum operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (sum? e) => boolean?
;;;   e: any/c
;;; Is e a sum?  checks the first item in the list for the plus symbol.
(define (sum? e)                  
  (and (pair? e)
       (eq? (first e) '+)))

;;; (addend e) => any
;;;   e: list?
;;; Addend of the sum e, is the second item in the list.
(define (addend e)	          
  (second e))

;;; (augend e) => list?
;;;   e: list?
;;; Augend of the sum e, is the third item in the list, or all items 3rd and beyond.
(define (augend e)
  (cond ((eq? (length e) 3)
         (third e))
        (else
         (simplify-recurse '+ (cddr e) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; product operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (cond ((eq? (length e) 3)
         (third e))
        (else
         (simplify-recurse '* (cddr e) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exponentiation operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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