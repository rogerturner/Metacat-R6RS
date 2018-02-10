#!r6rs

;;=============================================================================
;; Copyright (c) 2016 Roger Turner
;; This file is part of Metacat, which is (c) 1999, 2003 by James B. Marshall
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

(import (rnrs) (rnrs r5rs (6)) (rnrs io simple (6)))

;; Metacat-sufficient version of Chez Scheme's record-case
;; (see Chez Scheme Version 9 User’s Guide p122,
;;  https://cisco.github.io/ChezScheme/csug9.4/csug9_4.pdf)

(define-syntax record-case
  (lambda (x)
    (syntax-case x ()
      [ (_ expr c1 c2 ...)
        #`(let ([tag (car expr)])
            #,(let next-clause ([c1 #'c1] [cmore #'(c2 ...)])
                (if (null? cmore)
                    (syntax-case c1 (else)
                      [ (else body1 body2 ...) 
                        #'(begin body1 body2 ...)]
                      [ (key formals body1 body2 ...)
                        #'(if (eqv? tag 'key)
                              (apply (lambda formals body1 body2 ...) (cdr expr)))])
                    (syntax-case c1 ()
                      [ (key formals body1 body2 ...)
                        #`(if (eqv? tag 'key)
                              (apply (lambda formals body1 body2 ...) (cdr expr))
                              #,(next-clause (car cmore) (cdr cmore)))]))))])))

;; Simple formatted output
;; (Dybvig 2009 The Scheme Programming Language Fourth Edition p402,
;;  http://scheme.com/tspl4/examples.html#./examples:h6)

(define dofmt
  (lambda (p cntl args)
    (let ([nmax (- (string-length cntl) 1)])
      (let loop ([n 0] [a args])
        (if (<= n nmax)
            (let ([c (string-ref cntl n)])
                (if (and (char=? c #\~) (< n nmax))
                    (case (string-ref cntl (+ n 1))
                      [ (#\a #\A)
                        (display (car a) p)
                        (loop (+ n 2) (cdr a))]
                      [ (#\s #\S)
                        (put-string p (car a))
                        (loop (+ n 2) (cdr a))]
                      [ (#\%)
                        (newline p)
                        (loop (+ n 2) a)]
                      [ (#\~)
                        (put-char p #\~)
                        (loop (+ n 2) a)]
                      [else
                        (put-char p c)
                        (loop (+ n 1) a)])
                    (begin
                      (put-char p c)
                      (loop (+ n 1) a)))))))))

(define fprintf
  (lambda (p control . args)
    (dofmt p control args)))

(define format
  (lambda (control . args)
    (let-values ([(op g) (open-string-output-port)])
      (dofmt op control args)
      (g))))
  
(define *random-state* 48271)

(define random
  (let ((Lehmer-modulus 2147483647)
        (Lehmer-multiplier   48271))
    (lambda (x)
      (set! *random-state* (modulo (* *random-state* Lehmer-multiplier) Lehmer-modulus))
      (if (exact? x) 
        (modulo *random-state* x)
        (* x (/ *random-state* Lehmer-modulus))))))

(define random-seed
  (lambda x 
    (if (null? x)
      *random-state*
      (set! *random-state* (car x)))))
      
(define tanh
  (lambda (x)
    (/ (- (exp (* 2 x)) 1)
       (+ (exp (* 2 x)) 1))))
  
;; Stubs for Chez Scheme procedures
(define top-level-bound?        (lambda z #t))
(define current-directory       (lambda z #f))
(define collect                 (lambda z #f))
(define real-time               (lambda z 999))
(define waiter-prompt-and-read  (lambda z #f))
(define console-input-port      current-input-port)
(define console-output-port     current-output-port)
(define restore-current-state   (lambda z #f))

(define reset (lambda z (*terminate*)))

(define load (lambda z #f))
