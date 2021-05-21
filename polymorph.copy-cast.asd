;;;; polymorph.copy.asd

(asdf:defsystem #:polymorph.copy-cast
    :description "Copy/cast for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.5"
    :serial t
    :depends-on (#:compiler-macro #:polymorph.utility)
    :components ((:module
                  "src"
                  :serial t
                  :components
                   ((:file "package")
                    (:file "polymorph.copy-cast"))))

  :in-order-to ((asdf:test-op (asdf:test-op :polymorph.copy-cast/test))))

(asdf:defsystem #:polymorph.copy-cast/test
  :description "Unit tests for polymorph.copy-cast"
  :license "MIT"
  :serial t
  :depends-on (#:polymorph.copy-cast #:fiveam)
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "test"))))

  :perform (test-op (o s)
             (uiop:symbol-call '#:polymorph.copy-cast/test '#:test-polymorph.copy-cast)))
