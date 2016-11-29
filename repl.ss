(define run              ;; String ->
  (lambda (input)
     (let ((tokens (tokenize-string input)))
       (if (valid-token-list? tokens)
         (let ((problem
                 (cond
                   ((= (length tokens) 5) tokens)
                   ((= (length tokens) 3) `(,@tokens #f ,(random-seed)))
                   ((symbol? (4th tokens)) `(,@tokens ,(random-seed)))
                   ((number? (4th tokens))
                    `(,(1st tokens) ,(2nd tokens) ,(3rd tokens) #f ,(4th tokens))))))
           (set! %justify-mode% (exists? (4th problem)))
           (if* (andmap symbol? tokens)
             (randomize))
           (apply init-mcat problem)
           (run-mcat))
         (display "Invalid input!")))))

(define make-comment-reporter
  (lambda ()
    (lambda msg
      (let ((self (1st msg)))
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
                  (format " does \"~a\" change to?" target-sym)))
              'done))
          (add-comment (eliza-mode-lines lines)
            (display
              (if %eliza-mode%
                eliza-mode-lines
                lines))
            (newline))
          (else 'done))))))


(define swl:sync-display (lambda () 'done))

(define swl:version "1.3")