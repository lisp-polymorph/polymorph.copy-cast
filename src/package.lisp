;;;; package.lisp

(defpackage #:polymorph.copy-cast
  (:use #:cl #:polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:export #:deep-copy
           #:shallow-copy
           #:cast))
