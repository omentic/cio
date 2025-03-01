#lang racket
(require "../main.rkt")

;; The exception effect is papered over with a convenient throw function.
(define (throw msg)
  (suspend `(exn ,msg)))

;; The with-exception-handler procedure is defined on analogy to r7rs.
;; It takes in a handler procedure expecting an exception value,
;; and a zero-argument thunk being the computation to be handled.
;; This is nothing more than a convenience layer over with-effect-handler.
(define (with-exception-handler handler action)
  (with-effect-handler
    (λ (effect k)
      (match effect
        [`(exn ,msg) (handler msg)] ;; handle exception effects
        [_ (k (suspend effect))])) ;; non-exception effects
    action))

;; The catch macro is a useful syntactic construction. It functions like Java's try/catch.
;; It takes in a *computation*, which may raise an *exception*, and
;; a series of exception *patterns* and associated *handlers*.
(define-syntax-rule
  (catch computation [pattern handler ...] ...)
    (with-exception-handler
      (λ (msg) (match msg [pattern handler ...] ...))
      (thunk computation)))

(module+ main
  (define (safe-div a b)
    (when (zero? b)
      (throw "Division by Zero"))
    (/ a b))

  (catch (safe-div 1 (- 5 (+ 3 2)))
    ["Division by Zero"
      (println "That's undefined!")]
    [_ (println "Unknown error ~a.")])  )
