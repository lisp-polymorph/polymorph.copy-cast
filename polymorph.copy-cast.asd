;;;; polymorph.copy.asd

(asdf:defsystem #:polymorph.copy-cast
    :description "Copy/cast for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.5"
    :serial t
    :depends-on (#:adhoc-polymorphic-functions #:compiler-macro)
    :components ((:module
		  "src"
		  :serial t
		  :components
		  ((:file "package")
                   (:file "polymorph.copy-cast")))))
