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

(define run              ;; String ->
  (lambda (input)
     (let ( (tokens 
              (cond [(string? input) (tokenize-string input)]
                    [ else input])))
       (if (valid-token-list? tokens)
         (let ((problem
                 (cond
                   ((= (length tokens) 5) tokens)
                   ((= (length tokens) 3) `(,@tokens #f ,(random-seed)))
                   ((symbol? (fourth tokens)) `(,@tokens ,(random-seed)))
                   ((number? (fourth tokens))
                    `(,(first tokens) ,(second tokens) ,(third tokens) #f ,(fourth tokens))))))
           (set! %justify-mode% (exists? (fourth problem)))
           (if* (for-all symbol? tokens)
             (randomize))
           (apply init-mcat problem)
           (run-mcat))
         (display "Invalid input!")))))

(define *prev-eliza-para* "")

(define make-comment-reporter
  (lambda ()
    (lambda msg
      (let ((self (first msg)))
        (record-case (rest msg)
          (new-problem (initial-sym modified-sym target-sym answer-sym)
            (tell self 'add-comment
              (if %justify-mode%
                (list
                  (format "Let's see... \"~a\" changes to \"~a\", and"
                    initial-sym modified-sym)
                  (format " \"~a\" changes to \"~a\".  Hmm..."
                    target-sym answer-sym))
                (list
                  (format "Okay, if \"~a\" changes to \"~a\", what"
                    initial-sym modified-sym)
                  (format " does \"~a\" change to?  Hmm..." target-sym)))
              (if %justify-mode%
                (list
                  (format "Beginning justify run:  \"~a\" changes to \"~a\", and"
                    initial-sym modified-sym)
                  (format " \"~a\" changes to \"~a\"..."
                    target-sym answer-sym))
                (list
                  (format "Beginning run:  If \"~a\" changes to \"~a\", what"
                    initial-sym modified-sym)
                  (format " does \"~a\" change to?" target-sym))))
            'done)
          (add-comment (lines1 lines2)
            (let* ((paragraph1 (apply string-append lines1))
                   (paragraph2 (apply string-append lines2))
                   (original-para1 (string- paragraph1 " again")))
              (if %eliza-mode%
                (begin
                  (if (string=? original-para1 *prev-eliza-para*)
                    (display "Hmm...")
                    (display paragraph1))
                  (set! *prev-eliza-para* original-para1))
                (display paragraph2)))
            (newline))
          (else 'done))))))

(define string-
  (lambda (string match)
    (let ((start (string-contains string match)))
      (if start
        (string-append (substring string 0 start) 
                       (substring string (+ start (string-length match)) (string-length string)))
        string))))
        
(define string-contains
  (lambda (string match)
    (let* ((len (string-length match))
           (i-limit (- (string-length string) len)))
      (let loop ((i 0))
        (and (< i i-limit)
          (if (string=? match (substring string i (+ i len)))
            i
            (loop (+ i 1))))))))

