;;;; package.lisp

(defpackage #:polymorph.copy-cast
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:export #:deep-copy
           #:cast))
