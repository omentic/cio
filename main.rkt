#lang racket
(require racket/control)
(require racket/stxparam)
(require syntax/parse/define)
(provide with-effect-handler try resume resume/suspend suspend)

;; Define our own special prompt tag for use by this effects system...
;; so as to not interfere with anything else using continuations.
;; This should be instanciated whenever this module is imported.
(define cio (make-continuation-prompt-tag 'cio))

;; The suspend proc is one of several useful primitives.
;; It takes an *effect* (which is expected to be a *value*)
;; wrapped in a structural *tag*: i.e. `(yield ,(+ 1 x))
(define (suspend effect)
  ;; If no effectful continuation label exists, error out: all effects must be handled
  (unless (continuation-prompt-available? cio)
    (error 'suspend "unhandled effect ~a" effect))
  ;; Otherwise, capture the current continuation with call/comp, and abort up
  ;; to the nearest effect handler in the call stack with abort/cc
  (call/comp (λ (k) (abort/cc cio effect k)) cio))

;; A helpful parameter for ensuring resume is only used in appropriate contexts.
;; Does double duty in abstracting away the continuation invoked by resume.
(define-syntax-parameter try-k #f)
;; A necessary parameter to allow wrapping the call to resume in for deep handlers.
(define-syntax-parameter try-run #f)

;; The resume macro only functions within a handler.
;; It resumes the implicit continuation, possibly with a value.
(define-syntax (resume stx)
  (syntax-parse stx
    [(resume) ; deep handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #:attr run (syntax-parameter-value #'try-run)
        #:when (attribute run)
      #'(run k)]
    [(resume) ; shallow handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #'(k)]
    [(resume value) ; deep handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #:attr run (syntax-parameter-value #'try-run)
        #:when (attribute run)
      #'(let ([x value])
          (run (thunk (k x))))]
    [(resume value) ; shallow handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #:attr run (syntax-parameter-value #'try-run)
      #'(k value)]))

;; The resume/suspend macro only functions within a handler.
;; It resumes the implicit continuation with an *effect* that is immediately suspended.
;; This allows for the implementation of "bidirectional control flow".
(define-syntax (resume/suspend stx)
  (syntax-parse stx
    [(resume/suspend effect) ; deep handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #:attr run (syntax-parameter-value #'try-run)
        #:when (attribute run)
      #'(let ([x effect])
        (run (thunk (call-in-continuation k (thunk (suspend x))))))]
    [(resume/suspend effect) ; shallow handler
      #:fail-unless (syntax-parameter-value #'try-k)
        "must be used within the body of a try handler"
      #:attr k (syntax-parameter-value #'try-k)
      #'(call-in-continuation k (thunk (suspend effect)))]))

;; The with-effect-handler procedure is defined on analogy to r7rs's with-exception-handler.
;; It takes in a handler procedure expecting a value and a continuation,
;; and a zero-argument thunk being the computation to be handled.
;; This is no more than a layer over call/prompt.
;; As seen in the try macro definition, it is *very* close to shallow handlers.
(define (with-effect-handler handler action)
  (call/prompt (λ (_) (action)) cio handler (void)))

;; The try macro is another useful primitive.
;; It takes a *computation*, which may raise an *effect*, and
;; a series of effect *patterns* and associated *handlers*.
(define-syntax try
  (syntax-rules ()
    [(try #:shallow computation [pattern handler ...] ...)
      (with-effect-handler
        ;; If the computation jumps to the prompt tag: we have a suspended effect!
        (λ (effect k)
          ;; Match on the suspended effect, with the provided patterns/handlers.
          (match effect
            ;; If the effect matches a pattern, run the handler.
            ;; This may use the suspended continuation (if resume is called).
            ;; We must pass the necessary implicit parameters to the handler.
            [pattern (syntax-parameterize ([try-k #'k] [try-run #f])
              handler ...)] ...
            ;; If the effect matches no patterns, re-suspend up the call stack.
            ;; We must invoke k so as to not lose the initial site of suspension.
            ;; Alternatively, we could write (abort/cc cio effect k) directly.
            [_ (k (suspend effect))]))
        ;; Wrap the computation in call/prompt, and evaluate it.
        ;; If the computation completes without jumping to the prompt: return the value
        (thunk computation))]

    ;; Our implementation of deep handlers is much the same as the above,
    ;; but we wrap the handlers themselves in a new layer of call/prompt.
    [(try #:deep computation [pattern handler ...] ...)
      (let ()
        (define (run action)
          (with-effect-handler
            (λ (effect k)
              (match effect
                ;; Here, we wrap the handler body in our call/prompt again.
                [pattern (syntax-parameterize ([try-k #'k] [try-run #'run])
                  handler ...)] ...
                ;; We do NOT wrap the unhandled case in call/prompt, so as to not loop.
                [_ (k (suspend effect))]))
            action))
      (run (thunk computation)))]

    ;; I like deep handlers a lot more than shallow handlers. So they're the default.
    [(try computation [pattern handler ...] ...)
      (try #:deep computation [pattern handler ...] ...)]))

; References:
; https://github.com/tonyg/racket-effects/
; https://gist.github.com/jackfirth/027411d567385dadb3202bee75a847b4
; thx to notjack & co for racket help!!

(module+ test
  (require rackunit)

  ; Mutable state, for easier testing
  (define state null)
  (define (push x* x)
    (append x* (list x)))

  ; Shallow handlers
  (check-equal?
    (try #:shallow (for-each (λ (x) (suspend `(yield ,x))) '(1 2 3))
      [`(yield 1) "one"])
    "one")

  (check-equal?
    (try #:shallow (map (λ (x) (when (= x 1) (suspend `(yield ,x))) x) '(1 2 3))
      [`(yield ,x)
        (set! state x)
        (resume)])
    '(1 2 3))
  (check-equal? 1 state)

  ; Deep handlers
  (set! state null)
  (check-equal?
    (try (map (λ (x) (suspend `(yield ,x)) x) '(1 2 3))
      [`(yield ,x)
        (set! state (push state x))
        (resume)])
    '(1 2 3))
  (check-equal? state '(1 2 3))

  (set! state null)
  (check-equal?
    (try (map (λ (x) (suspend `(yield ,x))) '(1 2 3))
      [`(yield ,x)
        (set! state (push state x))
        (resume (+ 1 x))])
    '(2 3 4))
  (check-equal? state '(1 2 3))

  ; Nested shallow handlers
  (set! state null)
  (try #:shallow
    (try #:shallow
      (try #:shallow (for-each (λ (x) (suspend `(yield ,x))) '(1 2 3))
        [`(yield ,x)
          (set! state (push state (cons 1 x)))
          (resume)])
      [`(yield ,x)
        (set! state (push state (cons 2 x)))
        (resume)])
    [`(yield ,x)
      (set! state (push state (cons 3 x)))
      (resume)])
  (check-equal? state '((1 . 1) (2 . 2) (3 . 3)))

  (set! state null)
  (try #:shallow
    (try #:shallow
      (try #:shallow (for-each (λ (x) (suspend `(exception ,x))) '(1 2 3))
        [`(exception ,x) ; note the lack of resumes! explicit resumption is necessary
          (set! state (push state (cons 1 x)))])
      [`(exception ,x)
        (set! state (push state (cons 2 x)))])
    [`(exception ,x)
      (set! state (push state (cons 3 x)))])
  (check-equal? state '((1 . 1)))

  ; Nested deep handlers
  (set! state null)
  (check-equal?
    (try
      (try (map (λ (x) (suspend `(yield ,x))) '(1 2 3))
        [`(exception ,x) ; should not be caught here
          (set! state "failure!")])
      [`(yield ,x)
        (set! state (push state x))
        (resume (+ 1 x))])
    '(2 3 4))
  (check-equal? state '(1 2 3))

  ; Ensure multiple resumption works appropriately
  (set! state null)
  (try ; note that state is inlined by racket here...
    (set! state (push state (suspend `(hole))))
    [`(hole)
      (resume 1)
      (resume 2)
      (resume 3)])
  (check-equal? state '(3))

  (set! state null)
  (try ; evaluating suspend before state fixes it
    (let ([value (suspend `(hole))])
      (set! state (push state value)))
    [`(hole)
      (resume 1)
      (resume 2)
      (resume 3)])
  (check-equal? state '(1 2 3))

  (set! state null)
  (try
    (for-each (λ (x)
      (let ([value (suspend `(yield ,x))])
        (set! state (push state value)))) '(1 2 3))
    [`(yield ,x)
      (resume x)
      (resume x)])
  (check-equal? state '(1 2 3 3 2 3 3 1 2 3 3 2 3 3))

  (set! state null)
  (try #:shallow
    (try #:shallow
      (try #:shallow (for-each (λ (x) (suspend `(yield ,x))) '(1 2 3))
        [`(yield ,x)
          (set! state (push state x))
          (resume)
          (resume)])
      [`(yield ,x)
        (set! state (push state x))
        (resume)])
    [`(yield ,x)
      (set! state (push state x))])
  (check-equal? state '(1 2 3))

  ; Suspending from within a handler...
  ; (shallow and deep handlers should behave similarly here)
  (set! state null)
  (try #:shallow
    (try #:shallow (suspend `(yield 1))
      [`(yield ,x) (suspend `(yield ,x))])
    [`(yield ,x) (set! state x)])
  (check-equal? state 1)

  (set! state null)
  (try
    (try (suspend `(yield 1))
      [`(yield ,x) (suspend `(yield ,x))])
    [`(yield ,x) (set! state x)])
  (check-equal? state 1)

  ; Suspending within a call to resume
  (set! state null)
  (try
    (try
      (let ([x (suspend `(yield 1))])
        (set! state (push state x)))
      [`(yield 1)
        (set! state (push state 1))
        (resume (suspend `(yield 2)))]
      [`(yield ,x)
        (set! state #f)])
    [`(yield 2)
      (set! state (push state 2))
      (resume 3)])
  (check-equal? state '(1 2 3))

  ; resume/suspend
  (set! state null)
  (try #:shallow
    (try #:shallow (suspend `(yield 1))
      [`(yield ,x)
        (set! state (push state x))
        (resume/suspend `(yield ,(+ x 1)))])
    [`(yield ,x)
      (set! state (push state (+ x 1)))
      (resume)])
  (check-equal? state '(1 3))

  (set! state null)
  (try (suspend `(yield 1))
    [`(yield ,x)
      (unless (> x 2)
        (resume/suspend `(yield ,(+ x 1))))
      (set! state (push state x))])
  (check-equal? state '(3 2 1)))
