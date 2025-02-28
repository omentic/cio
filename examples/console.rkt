#lang racket
(require "../main.rkt")
(provide read write)

(define (write msg)
  (suspend `(write ,msg)))

(module+ main
  (define computation
    (thunk
      (write "hello")
      (write "world!")))

  (try (computation)
    [`(write ,msg)
      (println msg)
      (resume)])

  (try (computation)
    [`(write ,msg)
      (println (format "I see you tried to print '~a'." msg))
      (resume)]))

(define (read)
  (suspend `(read)))

(module+ main
  (define computation-ii
    (thunk
      (write "Hello-- oh, I don't know your name.")
      (write (string-append "Hello " (read)))))

  (try (computation-ii)
    [`(write ,msg)
      (println msg)
      (resume)]
    [`(read)
      (println "What's your name?")
      (resume (read-line))]))
