# polymorph.copy

- Cast
``` common-lisp
(define-polymorphic-function cast (object type))
```
Casts an object to a specified type. Type should always be a symbol. Non-mutating, i.e. always returns a new object.

- Copy
``` common-lisp
(define-polymorphic-function deep-copy (object))
(define-polymorphic-function shallow-copy (object))
```
Deep-copy copies object recursively, while shallow-copy only copies the topmost container, leaving insides as references. Returns object of precisely the same type as the original one.
