;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================

;; random seeds cannot be bigger than this number in Chez Scheme
(define *largest-random-seed* 4294967295)

(define concatenate-symbols
  (lambda symbols
    (string->symbol (apply string-append (map symbol->string symbols)))))

(define printf
  (let ((out (current-output-port)))
    (lambda args
      (apply fprintf (cons out args)))))

(define newline
  (let ((out (current-output-port)))
    (lambda ()
      (fprintf out "~%"))))

;;----------------------------------------------------------------------------------
;; Utility Macros

(define *control-panel* (lambda z #f))

(define-syntax mcat
  (syntax-rules ()
   [ (_ token ...)
     (if (valid-token-list? '(token ...))
         (tell *control-panel* 'run-new-problem '(token ...)))]))

;; valid token list formats:
;; (sym sym sym)
;; (sym sym sym sym)
;; (sym sym sym num)
;; (sym sym sym sym num)

(define valid-token-list?
  (lambda (tokens)
    (and (list? tokens)
     (>= (length tokens) 3)
     (symbol? (first tokens))
     (symbol? (second tokens))
     (symbol? (third tokens))
     (or (= (length tokens) 3)
         (and (= (length tokens) 4)
          (symbol-or-valid-number? (fourth tokens)))
         (and (= (length tokens) 5)
          (symbol? (fourth tokens))
          (valid-number? (fifth tokens)))))))

(define valid-number?
  (lambda (num)
    (and (integer? num)
         (> num 0)
     (<= num *largest-random-seed*))))

(define symbol-or-valid-number?
  (lambda (token)
    (or (symbol? token) (valid-number? token))))

(define-syntax for*
  (lambda (x)
    (syntax-case x (each in from to do)
      ( (_ each (formal ...) in (exp ...) do body ...)
        (andmap symbol? '(formal ...))
        #`(for-each (lambda (formal ...) body ...) exp ...))
      ( (_ each formal in exp do body ...)
        (symbol? 'formal)
        #`(for-each (lambda (formal) body ...) exp))
      ( (_ formal from exp1 to exp2 do body ...)
        #`(for* each formal in
            (let ((exp1-value exp1)
                  (exp2-value exp2))
              (if (< exp2-value exp1-value)
                '()
                (map (lambda (n) (+ n exp1-value))
                  (ascending-index-list (add1 (- exp2-value exp1-value)))))) do
            body ...)))))

(define-syntax for-each-vector-element*
  (syntax-rules (do)
    ( (_ (v i) do exp ...)
      (for* each i in (ascending-index-list (vector-length v)) do exp ...))))

(define-syntax for-each-table-element*
  (syntax-rules (do)
    ( (_ (t i j) do exp ...)
      (for-each-vector-element* (t i) do
        (for* each j in (ascending-index-list (vector-length (vector-ref t i))) do
          exp ...)))))

(define-syntax repeat*
  (syntax-rules (times forever until)
    ( (_ n times exp ...)
      (let ((thunk (lambda () exp ...))
            (i n))
        (letrec ((loop (lambda () (if* (> i 0) (thunk) (set! i (sub1 i)) (loop)))))
          (loop))))
    ( (_ forever exp ...)
      (let ((thunk (lambda () exp ...)))
        (letrec ((loop (lambda () (thunk) (loop))))
          (loop))))
    ( (_ until condition exp ...)
      (let ((done? (lambda () condition))
            (thunk (lambda () exp ...)))
        (letrec ((loop (lambda () (if* (not (done?)) (thunk) (loop)))))
          (loop))))))

(define-syntax if*
  (syntax-rules ()
    ( (_ test exp ...) 
      (if test (begin exp ...) (void)))))
    
(define-syntax stochastic-if*
  (syntax-rules ()
    ( (_ prob exp ...)
      (let ((prob-thunk (lambda () prob))
            (exps-thunk (lambda () exp ...)))
        (let ((coin-flip (random 1.0)))
          (if (< coin-flip (prob-thunk)) (exps-thunk) (void)))))))

(define-syntax continuation-point*
  (syntax-rules ()
    ( (_ name exp ...)
      (call/cc (lambda (name) exp ...)))))

(define-syntax say
  (syntax-rules ()
    ( (_ x ...) 
      (if* %verbose% (say-object x) ... (newline)))))

(define-syntax say!
  (syntax-rules ()
    ( (_ x ...) 
      (begin (say-object x) ... (newline)))))

(define-syntax vprintf
  (syntax-rules ()
    ( (_ x ...)
      (if* %verbose% (printf x ...)))))

(define-syntax vprint
  (syntax-rules ()
    ( (_ x ...)
      (if* %verbose% (print x ...)))))

;;----------------------------------------------------------------------------------
;; Codelet Macros

(define-syntax post-codelet* 
  (syntax-rules (urgency:)
    ( (_ urgency: rel-urg codelet-type arg ...)
      (tell *coderack* 'post (tell codelet-type 'make-codelet rel-urg arg ...)))))

(define fizzle #f)

(define-syntax define-codelet-procedure*
  (syntax-rules (lambda)
    ( (_ codelet-type (lambda formals exp ...))
      (tell codelet-type 'set-codelet-procedure
        (lambda formals
          (continuation-point* return
            (set! fizzle (lambda () (set! fizzle #f) (return 'done)))
            (say "----------------------------------------------")
            (say "In " 'codelet-type " codelet...")
            exp ...))))))
