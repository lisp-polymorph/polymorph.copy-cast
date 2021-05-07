;;;; package.lisp

(defpackage #:polymorph.copy-cast
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:export #:deep-copy
           #:shallow-copy
           #:cast))
