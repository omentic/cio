#lang racket
(require racket/control)
(require racket/stxparam)
(require syntax/parse/define)
(provide try resume resume/suspend suspend)

;; Define our own special prompt tag for use by this effects system...
;; so as to not interfere with anything else using continuations
;; This should be instanciated whenever this module is imported.
(define cio (make-continuation-prompt-tag 'cio))

;; The suspend proc is one of three or four useful primitives.
;; It takes an *effect* (which is expected to be a *value*)
;; wrapped in a *tag*: i.e. `(yield ,(+ 1 x))
(define (suspend effect)
  ;; If no effectful continuation label exists, error: all effects must be handled
  (unless (continuation-prompt-available? cio)
    (error 'suspend "unhandled effect ~a" effect))
  ;; Otherwise, capture the current continuation with call/comp, and abort up
  ;; to the nearest effect handler in the call stack with abort/cc
  (call/comp (λ (continuation) (abort/cc cio effect continuation)) cio))

;; A helper parameter for ensuring (resume) is only used in appropriate contexts.
(define-syntax-parameter try-continuation #f)

;; The resume macro only functions within a handler.
;; It resumes the implicit continuation, possibly with a value.
(define-syntax-parse-rule (resume (~optional value))
  #:fail-unless (syntax-parameter-value #'try-continuation)
  "resume must be used within the body of a try handler"
  #:with continuation (syntax-parameter-value #'try-continuation)
  (continuation (~? value)))

;; The resume/suspend macro only functions within a handler.
;; It resumes the implicit continuation with an *effect* that is immediately suspended.
;; This allows for the implementation of "bidirectional control flow".
(define-syntax-parse-rule (resume/suspend effect)
  #:fail-unless (syntax-parameter-value #'try-continuation)
  "resume must be used within the body of a try handler"
  #:with continuation (syntax-parameter-value #'try-continuation)
  (call-in-continuation continuation (λ () (suspend effect))))

;; The try macro is one of three useful primitives.
;; It takes a *computation*, which may raise an *effect*, and
;; a series of effect *patterns* and associated *handlers*.
(define-syntax try
  (syntax-rules ()
    [(try #:shallow computation [pattern handler ...] ...)
      ;; Wrap the computation in call/prompt, and evaluate it.
      ;; If the computation completes without jumping to the prompt: return the value
      (call/prompt (λ (_) computation) cio ; need to inject cio in??
        ;; If the computation jumps to the prompt tag: we have a suspended effect!
        (λ (effect continuation)
          ;; Match on the suspended effect, with the provided patterns/handlers.
          (match effect
            ;; If the effect matches a pattern, run the handler.
            ;; This may use the suspended continuation (if resume is called).
            [pattern (syntax-parameterize ([try-continuation #'continuation]) handler ...)] ...
            ;; If the effect matches no patterns, re-suspend up the call stack.
            [_ (abort/cc cio effect continuation)]))
        (void))]

    ;; We do the same as above for deep handlers (the default), but wrap the handlers
    ;; themselves in a new layer of call/prompt.
    [(try #:deep computation [pattern handler ...] ...)
      (letrec ([run (λ (action)
        (call/prompt action cio
          (λ (effect continuation)
            (match effect
              [pattern (syntax-parameterize ([try-continuation #'continuation])
                ;; Here, we wrap the handler body in our call/prompt again.
                (run (λ (_) handler)) ...)] ...
              ;; We do NOT wrap the unhandled case in call/prompt, so as to not loop.
              [_ (abort/cc cio effect continuation)]))
          (void)))])
        (run (λ (_) computation)))]

    ;; I like deep handlers a lot more than shallow handlers. So they're the default.
    [(try computation [pattern handler ...] ...)
      (try #:deep computation [pattern handler ...] ...)]))

; References:
; https://github.com/tonyg/racket-effects/
; https://gist.github.com/jackfirth/027411d567385dadb3202bee75a847b4
; thx to notjack & co for racket help!!

(module+ test
  (require rackunit)

  ; Shallow handlers
  (check-equal? "one"
    (try #:shallow (map (λ (x) (suspend `(yield ,x)) x) '(1 2 3))
      [`(yield 1) "one"]))

  (check-equal? '(1 2 3)
    (try #:shallow (map (λ (x) (if (= x 1) (begin (suspend `(yield ,x)) x) x)) '(1 2 3))
      [`(yield ,x) (set! state x) (resume)]))
  (check-equal? 1 state)

  ; Deep handlers
  (define state #f)

  (set! state null)
  (check-equal? '(1 2 3)
    (try (map (λ (x) (begin (suspend `(yield ,x)) x)) '(1 2 3))
      [`(yield ,x) (set! state (append state (list x))) (resume)]))
  (check-equal? '(1 2 3) state)

  (set! state null)
  (check-equal? '(2 3 4)
    (try (map (λ (x) (suspend `(yield ,x))) '(1 2 3))
      [`(yield ,x) (set! state (append state (list x))) (resume (+ 1 x))]))
  (check-equal? '(1 2 3) state)

  (set! state null)
  (check-equal? '(3 4 5)
    (try
      (try (map (λ (x) (suspend `(yield ,x))) '(1 2 3))
        [`(bield ,x) (set! state #f) (resume (+ 1 x))])
      [`(yield ,x) (set! state (append state (list x))) (resume (+ 2 x))]))
  (check-equal? '(1 2 3) state))
