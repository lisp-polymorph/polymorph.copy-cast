;;;; package.lisp

(defpackage #:polymorph.copy-cast
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:export #:deep-copy
           #:shallow-copy
           #:cast))
