#lang racket
(require "../main.rkt")

(define (write msg)
  (suspend `(write ,msg)))

(define computation
  (λ () (begin (write "hello") (write "world!"))))

(try (computation)
  [`(write ,msg)
    (println msg) (resume)])

(try (computation)
  [`(write ,msg)
    (println (format "I see you tried to print '~a'. Not so fast!" msg)) (resume)])

(define (read)
  (suspend `(read)))

(define computation-ii
  (λ () (begin (write "hello") (read))))

(try (computation-ii)
  [`(write ,msg)
    (println msg) (resume)]
  [`(read)
    (println "Alice") (resume)])

(try (computation-ii)
  [`(write ,msg)
    (println msg) (resume)]
  [`(read)
    (println "(what's your name)")
    (println (read-line)) (resume)])
