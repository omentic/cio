# cio: a structural effect handlers library

`cio` is an (untyped) structural effect handlers library for Racket.

This was primarily a learning project for me to understand how effect handlers and `call/cc` and the like slot together. If you want to use it, go ahead! The implementation is fairly minimal. Be warned it is not well-tested, and not guaranteed to behave with other continuation-based libraries (though it should).

The implementation is well-commented, with the intention of being useful to students of continuations.

```racket
(define (write msg)
  (suspend `(write ,msg)))

(define computation
  (thunk (write "hello") (write "world!")))

(try (computation)
  [`(write ,msg)
    (println msg) (resume)])

(try (computation)
  [`(write ,msg)
    (println (format "I see you tried to print '~a'." msg)) 
    (resume)])

(define (read)
  (suspend `(read)))

(define another-computation
  (thunk (write (string-append "Hello, " (read)))))

(try (another-computation)
  [`(write ,msg)
    (println msg) 
    (resume)]
  [`(read)
    (println "Hello-- what's your name?")
    (resume (read-line))])
```

## design

The design of `cio` is primarily modelled after [Effekt](https://effekt-lang.org/): though lacking the types, of course. \
`cio` provides several primitives: `try`, `suspend`, `resume`, `resume/suspend`, and `with-effect-handler`.

`try` steps a computation to a value. If in the process of that computation, an effect is raised by `suspend`, and an appropriate *handler* (case) is provided for it within the `try` body, the effect is handled there and the computation (may be) resumed.

There is a distinction between *deep* and *shallow* `try` handlers. `cio` supports both - deep handlers are used by default, but shallow handlers can be used by inserting the `#:shallow` parameter immediately following `try`. Shallow handlers only trigger once, while deep handlers will handle future effects after any resumption. Deep handlers are considerably more useful and intuitive, and combined with mutable state can easily model shallow handlers - thus they are the default.

`suspend` takes a value, treated as an *effect*, and pops it up the call stack to the nearest `try` handler. It should be noted that suspend takes a *value*. `cio` is structural, and effect handlers do no more than match upon values. If the user wishes to distinguish between different kinds of effects (which they probably should), the value in `suspend` must be wrapped in a tag corresponding to a tag in a handler.

`resume` *resumes* a computation, optionally with a value. `resume/suspend` resumes a computation with an effect. They are only usable within the handlers of a `try` block. When an effect is raised from the computation of a `try` block, and caught by a handler, a *continuation* to the rest of the computation is bound to a hidden continuation variable, which `resume` operates upon. This means that it can be difficult to operate on an external `try` block within another nested `try` block - however, `resume` is expanded before being packed up in a function, meaning this can be worked around with mutable state, [Effekt-style](https://github.com/effekt-lang/effekt/issues/108).

Resumption is an extremely powerful feature. Suspension alone allows the implementation of exceptions. Suspension and resumption without a value allows the implementation of generators. Suspension and resumption with a value allows the implementation of async/await. Suspension and resumption with *another effect* allows the implementation of *bidirectional control flow*. This makes for a very powerful - possibly too powerful - abstraction over all non-local control flow. Whether this can be done in a controlled and well-typed fashion is an active area of research.

Finally, `with-effect-handler` is a lower-level helper function for `try` blocks. It is implemented on analogy to r7rs's `with-exception-handler`.

## bibliography

A lot of wonderful papers on effect handlers have been published, most of them in the last couple of years. If you are interested in learning more, I recommend reading the following papers / listening to the following talks, in order:

1. [Deliminated Continuations, Demystified](https://www.youtube.com/watch?v=TE48LsgVlIU) (talk only)
2. [Effects as Capabilities](https://dl.acm.org/doi/pdf/10.1145/3428194) ([talk](https://www.youtube.com/watch?v=ITRyzJadgMw))
3. [Handling Bidirectional Control Flow](https://dl.acm.org/doi/pdf/10.1145/3428207) ([talk](https://www.youtube.com/watch?v=RLTEuZNtRCc))
4. [Effects, Capabilities, and Boxes](https://dl.acm.org/doi/pdf/10.1145/3527320) ([talk](https://www.youtube.com/watch?v=P7X5Qy9KYLU))
5. [Continuing WebAssembly with Effect Handlers](https://arxiv.org/pdf/2308.08347) ([talk](https://www.youtube.com/watch?v=2iiVhzzvnGA))
6. [Soundly Handling Linearity](https://dl.acm.org/doi/pdf/10.1145/3632896) ([talk](https://www.youtube.com/watch?v=sHGrKEoWtm8))
7. [Lexical Effect Handlers, Directly](https://dl.acm.org/doi/pdf/10.1145/3689770)
