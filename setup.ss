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

(define *codelet-count* 0)
(define *temperature* 0)

(define *workspace-window* (lambda z #f))
(define *slipnet-window* (lambda z #f))
(define *coderack-window* (lambda z #f))
(define *themespace-window* (lambda z #f))
(define *top-themes-window* (lambda z #f))
(define *bottom-themes-window* (lambda z #f))
(define *vertical-themes-window* (lambda z #f))
(define *memory-window* (lambda z #f))
(define *comment-window* (lambda z #f))
(define *trace-window* (lambda z #f))
(define *temperature-window* (lambda z #f))
(define *EEG* (lambda z #f))
(define *EEG-window* (lambda z #f))

;; Default configuration:
(define %eliza-mode% #t)
(define %justify-mode% #f)
(define %self-watching-enabled% #t)
(define %verbose% #f)
(define %workspace-graphics% *gui*)
(define %slipnet-graphics% *gui*)
(define %coderack-graphics% *gui*)
(define %codelet-count-graphics% *gui*)
(define %highlight-last-codelet% *gui*)
(define %nice-graphics% *gui*)

(define %num-of-flashes% 1)
(define %flash-pause% 1)
(define %snag-pause% 1)
(define %codelet-highlight-pause% 1)
(define %text-scroll-pause% 1)

(define *repl-thread* #f)

(define setup
  (lambda args
    (if *gui*
      (if (null? args)
        (setup-gui)
        (setup-gui args)))
    (if *repl*
      (set! *comment-window* (make-comment-reporter)))))

;;------------------------------------------------------------------
;; User-interface commands

(define eliza-mode-on
  (lambda ()
    (set! %eliza-mode% #t)
    (tell *comment-window* 'switch-modes)
    'ok))

(define eliza-mode-off
  (lambda ()
    (set! %eliza-mode% #f)
    (tell *comment-window* 'switch-modes)
    'ok))

(define slipnet-on
  (lambda ()
    (set! %slipnet-graphics% #t)
    (if* (not *display-mode?*)
      (tell *slipnet-window* 'restore-current-state))
    'ok))

(define slipnet-off
  (lambda ()
    (set! %slipnet-graphics% #f)
    (if* (not *display-mode?*)
      (tell *slipnet-window* 'blank-window))
    'ok))

(define coderack-on
  (lambda ()
    (set! %coderack-graphics% #t)
    (if* (not *display-mode?*)
      (tell *coderack-window* 'restore-current-state))
    'ok))

(define coderack-off
  (lambda ()
    (set! %coderack-graphics% #f)
    (if* (not *display-mode?*)
      (tell *coderack-window* 'blank-window "Coderack"))
    'ok))

(define codelet-counts-on
  (lambda ()
    (set! %codelet-count-graphics% #t)
    (tell *coderack-window* 'initialize)
    'ok))

(define codelet-counts-off
  (lambda ()
    (set! %codelet-count-graphics% #f)
    (tell *coderack-window* 'initialize)
    'ok))

(define clearmem
  (lambda ()
    (tell *memory* 'clear)
    'ok))

(define verbose-on
  (lambda ()
    (set! %verbose% #t)
    (if* (not (tell *control-panel* 'verbose-mode?))
      (tell *control-panel* 'toggle-verbose-mode))
    'ok))

(define verbose-off
  (lambda ()
    (set! %verbose% #f)
    (if* (tell *control-panel* 'verbose-mode?)
      (tell *control-panel* 'toggle-verbose-mode))
    'ok))

(define speed
  (lambda ()
    (printf "Current speed settings:~n")
    (printf "  %num-of-flashes%           ~a~%" %num-of-flashes%)
    (printf "  %flash-pause%              ~a ms~%" %flash-pause%)
    (printf "  %snag-pause%               ~a ms~%" %snag-pause%)
    (printf "  %codelet-highlight-pause%  ~a ms~%" %codelet-highlight-pause%)
    (printf "  %text-scroll-pause%        ~a ms~%" %text-scroll-pause%)))

;;----------------------------------------------------------------------
;; Probability distributions

(define make-probability-distribution
  (lambda (values distribution-frequency-values)
    (lambda msg
      (record-case (rest msg)
       (object-type () 'probability-distribution)
       (choose-value () (stochastic-pick values distribution-frequency-values))
       (else (delegate msg base-object))))))


(define %very-low-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 5  150  5   2   1   1   1   1   1   1)))


(define %low-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 2   5  150  5   2   1   1   1   1   1)))


(define %medium-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   2   5  150  5   2   1   1   1   1)))


(define %high-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   1   2   5  150  5   2   1   1   1)))


(define %very-high-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   1   1   2   5  150  5   2   1   1)))

;;--------------------------------------------------------------------------------
;; command line parser

(define tokenize-string
  (lambda (input)
    (let ((chars (map char-downcase (string->list input))))
      (define consume-noise
       (lambda (buffer tokens chars)
         (cond
           ((null? chars) (reverse tokens))
           ((char-noise? (first chars))
            (consume-noise buffer tokens (rest chars)))
           ((char-alphabetic? (first chars))
            (consume-letters (cons (first chars) buffer) tokens (rest chars)))
           ((char-numeric? (first chars))
            (consume-digits (cons (first chars) buffer) tokens (rest chars)))
           (else 'error))))
      (define consume-letters
       (lambda (buffer tokens chars)
         (cond
           ((null? chars)
            (let ((new-token (string->symbol (list->string (reverse buffer)))))
              (reverse (cons new-token tokens))))
           ((char-noise? (first chars))
            (let ((new-token (string->symbol (list->string (reverse buffer)))))
              (consume-noise '() (cons new-token tokens) (rest chars))))
           ((char-alphabetic? (first chars))
            (consume-letters (cons (first chars) buffer) tokens (rest chars)))
           (else 'error))))
      (define consume-digits
       (lambda (buffer tokens chars)
         (cond
           ((null? chars)
            (let ((new-token (string->number (list->string (reverse buffer)))))
              (reverse (cons new-token tokens))))
           ((char-noise? (first chars))
            (let ((new-token (string->number (list->string (reverse buffer)))))
              (consume-noise '() (cons new-token tokens) (rest chars))))
           ((char-numeric? (first chars))
            (consume-digits (cons (first chars) buffer) tokens (rest chars)))
           (else 'error))))
      (consume-noise '() '() chars))))

(define char-noise?
  (lambda (char)
    (and (not (char-alphabetic? char))
     (not (char-numeric? char)))))
