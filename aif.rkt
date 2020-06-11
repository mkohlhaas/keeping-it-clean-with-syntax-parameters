#lang racket

#|
Document: "Keeping it Clean with Syntax Parameters" by Eli Barzilay

; Question: “How do I break hygiene when I need to?”
; Answer: “You don't need to!”

Key forms worth to keep in mind:
- define-syntax-parameter (declare of adjustable binding)
- syntax-parameterize (adjust adjustable bindings)
- make-rename-transformer
- syntax-id-rules
|#

(require racket/stxparam)

#;
(define-syntax aif
  (syntax-rules ()
    [(aif test then else)
     (let ([it test]) ; Bad luck. 'it' is hygienic! We have two 'it's. One in aif and one in 'then'.
       (if it then else))]))

; declare adjustable binding, here called 'it'
; 'it' can be used inside a macro, but you must define what that means by using syntax-parametrize.
; define-syntax-parameter acts like normal define-syntax but can be updated to use a new transformer with syntax-parameterize
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error
     (syntax-e stx)
     "can only be used inside a macro")))

#;
it

; this is allowed ('it' is not somehow overwritten by syntax-parameterize)
; here 'it' is just a normal local binding
(let ([it "It's me"])
  it)

; creating proper binding of the auxiliary identifier 'it'
; 'it' is then referred to like any other binding
#;
(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        ; inside the true expression we allow 'it'
        (syntax-parameterize
            ; 'it' is an alias for the local'tmp'
            ([it (make-rename-transformer #'tmp)])
          true-expr)
        false-expr)))

; using syntax-id-rules instead of make-rename-transformer
(define-syntax aif
  (syntax-rules ()
    [(aif test then else)
     (let ([tmp test])
       ; 'it' is an alias for the local 'tmp' using syntax-id-rules
       ; in other words: 'it' maps to 'tmp'
       (syntax-parameterize ([it (syntax-id-rules () [_ tmp])])
         (if tmp then else)))]))

(aif 10 (displayln it) (println "Who am I?"))
(aif #f (displayln it) (println "I'm false!"))
; can't use 'it' in the 'then' part
; (aif #f (displayln it) (println it))
(aif (member 5 '(13 5 7 9 11))
     (apply + it)
     (println "No, there is no 5!"))
