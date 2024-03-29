;;;; polymorph.copy-cast.lisp

(in-package #:polymorph.copy-cast)
;;; Cast

;; TODO casts to strings -- should probably try to cast stuff to characters as well? But how?
;; There are at least 2 possibilities -- 1 turns into #\1 or into (code-char 1).
(define-polymorphic-function cast (object type) :overwrite t
  :documentation "Cast an object to a specified type. Type should always be a symbol.
  Non-mutating, i.e. always return a new object.")



(defpolymorph cast ((object number) (type (member number complex real float
                                                  single-float double-float
                                                  long-float short-float ratio
                                                  rational bignum)))
  number
  (coerce object type))


(defpolymorph cast ((object number) (type (eql integer))) number
  (declare (ignorable type))
  (nth-value 0 (truncate object)))

(defpolymorph cast ((object number) (type list)) number
  (coerce object type))

#||
(defpolymorph-compiler-macro cast (number (member number complex real
                                                  float single-float double-float
                                                  long-float short-float ratio rational
                                                  integer fixnum bignum))
  (object type)
  `(the ,(if (constantp type) (eval type) 'number) (coerce ,object ,type)))
||#


(defpolymorph cast ((object list) (type (eql boolean))) boolean
  (declare (ignorable type))
  (not (null object)))


(defpolymorph cast ((object (mod 1114112)) (type (eql character))) character
  (declare (ignorable type))
  (code-char object))

(defpolymorph cast ((object character) (type (eql integer))) (mod 1114112)
  (declare (ignorable type))
  (char-code object))


(defpolymorph cast ((object bit) (type (eql boolean))) boolean
  (declare (ignorable type))
  (cl:= object 1))

(defpolymorph cast ((object boolean) (type (eql bit))) bit
  (declare (ignorable type))
  (if object 1 0))





(define-polymorphic-function deep-copy (object) :overwrite t
  :documentation "Deep-copy copies object recursively.
  Return object has the same type as the original one.")
(define-polymorphic-function shallow-copy (object) :overwrite t
  :documentation "Copy the topmost container, leaving insides as references.
  Return object has precisely the same type as the original one.")





;; Deep-copy  --- is mediocre rn



;; Immutables
(defpolymorph deep-copy ((o number)) number
  o)

(defpolymorph deep-copy ((o character)) character
  o)

(defpolymorph deep-copy ((o symbol)) symbol
  o)



;;Arrays
(defpolymorph (deep-copy :inline t) ((o simple-array)) simple-array
  (let ((out (make-array (array-dimensions o)
                         :element-type (array-element-type o))))
    (loop :for i :below (array-total-size o)
          :do (setf (row-major-aref out i)
                    (deep-copy (row-major-aref o i))))
    out))

(defpolymorph-compiler-macro deep-copy (simple-array)
    (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (let ((formtype (with-type-info (type () env) o type))
            (i (gensym "I")))
        (let ((out (gensym "OUT")))
          (once-only (o)
            `(the ,formtype
                  (let ((,out (make-array ,(if (constant-array-dimensions-p o-dim env)
                                             `'(,@o-dim)
                                             `(array-dimensions ,o))
                                          :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt))))
                    (declare (type ,formtype ,out))
                    (loop :for ,i :below (array-total-size ,o)
                       :do (setf (row-major-aref ,out ,i)
                                 (deep-copy (the ,o-elt (row-major-aref ,o ,i)))))
                    ,out))))))))

(defpolymorph (deep-copy :inline t) ((o (and vector (not simple-array)))) (and vector (not simple-array))
  ;; Could consider more options like displacements
  (let ((r (make-array (array-total-size o)
                       :element-type (array-element-type o)
                       :adjustable t
                       :fill-pointer (when (array-has-fill-pointer-p o)
                                       (fill-pointer o)))))
    (loop :for i :below (length o)
          :do (setf (aref r i)
                    (deep-copy (aref o i))))
    r))

(defpolymorph-compiler-macro deep-copy ((and vector (not simple-array))) (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (with-type-info (o-type () env) o
        `(the ,o-type
              ,(once-only (o)
                 `(let ((r (make-array ,(if (constant-array-dimensions-p o-dim env)
                                            `'(,@o-dim)
                                            `(array-dimensions ,o))
                                       :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt)
                                       :adjustable t
                                       :fill-pointer
                                         (when (array-has-fill-pointer-p ,o)
                                           (fill-pointer ,o)))))
                    (declare (type ,o-type ,o r))
                    (loop :for i :below (length ,o)
                       :do (setf (aref r i)
                                 (the ,o-elt
                                      (deep-copy (the ,o-elt (aref ,o i))))))
                    r)))))))

(defpolymorph (deep-copy :inline t) ((o array)) array
  (let ((r (make-array (array-dimensions o)
                       :element-type (array-element-type o)
                       :adjustable t)))
    (loop :for i :below (array-total-size o)
          :do (setf (row-major-aref r i)
                    (deep-copy (row-major-aref o i))))
    r))

(defpolymorph-compiler-macro deep-copy (array) (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (with-type-info (o-type () env) o
        `(the ,o-type
              ,(once-only (o)
                 `(let ((r (make-array ,(if (constant-array-dimensions-p o-dim env)
                                            `'(,@o-dim)
                                            `(array-dimensions ,o))
                                       :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt)
                                       :adjustable t)))
                    (declare (type ,o-type ,o r))
                    (loop :for i :below (array-total-size ,o)
                       :do (setf (row-major-aref r i)
                                 (the ,o-elt
                                      (deep-copy (the ,o-elt (row-major-aref ,o i))))))
                    r)))))))


;;Cons (and lists? unsure)
(defpolymorph deep-copy ((o cons)) cons
  (cons (deep-copy (car o)) (deep-copy (cdr o))))


(defpolymorph-compiler-macro deep-copy (cons) (o &environment env)
  (with-type-info (type () env) o
    `(the ,type
          ,(once-only (o)
             `(cons (deep-copy (car ,o))
                    (deep-copy (cdr ,o)))))))

;; Hash-table
(defpolymorph deep-copy ((o hash-table)) (values hash-table &optional)
  (let ((copy (make-hash-table :test (hash-table-test o)
                               :size (hash-table-size o)
                               :rehash-size (hash-table-rehash-size o)
                               :rehash-threshold (hash-table-rehash-threshold o))))
    (loop :for k :being :the :hash-keys :in o
            :using (hash-value v)
          :do (setf (gethash (deep-copy k) copy) (deep-copy v)))
    copy))


;;Shallow copy
(defpolymorph shallow-copy ((o number)) number
              o)

(defpolymorph shallow-copy ((o character)) character
  o)

(defpolymorph shallow-copy ((o symbol)) symbol
  o)


;;Arrays
(defpolymorph (shallow-copy :inline t) ((o simple-array)) simple-array
  (let ((r (make-array (array-dimensions o)
                       :element-type (array-element-type o))))
    (loop :for i :below (array-total-size o)
          :do (setf (row-major-aref r i)
                    (row-major-aref o i)))
    r))

(defpolymorph-compiler-macro shallow-copy (simple-array) (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (with-type-info (o-type () env) o
        `(the ,o-type
              ,(once-only (o)
                 `(let ((r (make-array ,(if (constant-array-dimensions-p o-dim env)
                                           `'(,@o-dim)
                                            `(array-dimensions ,o))
                                       :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt))))
                    (declare (type ,o-type ,o r))
                    (loop :for i :below (array-total-size ,o)
                       :do (setf (row-major-aref r i)
                                 (the ,o-elt
                                      (row-major-aref ,o i))))
                    r)))))))

(defpolymorph (shallow-copy :inline t) ((o (and vector (not simple-array)))) (and vector (not simple-array))
  ;; Could consider more options like displacements
  (let ((r (make-array (array-total-size o)
                       :element-type (array-element-type o)
                       :adjustable t
                       :fill-pointer (fill-pointer o)
                       :initial-contents o)))
          ;(loop :for i :below (length o)
          ;      :do (setf (aref r i)
          ;                (aref o i))))
    r))

(defpolymorph-compiler-macro shallow-copy ((and vector (not simple-array))) (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (with-type-info (o-type () env) o
        (once-only (o)
          `(the ,o-type (make-array ,(if (constant-array-dimensions-p o-dim env)
                                         `'(,@o-dim)
                                         `(array-dimensions ,o))
                                    :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt)
                                    :adjustable t
                                    :fill-pointer (fill-pointer ,o)
                                    :initial-contents ,o)))))))
                       ;(loop :for i :below (length ,o)
                       ;   :do (setf (aref r i)
                       ;             (the ,o-elt
                       ;                  (aref ,o i)))

(defpolymorph (shallow-copy :inline t) ((o array)) array
  (let ((r (make-array (array-dimensions o)
                       :element-type (array-element-type o)
                       :adjustable t)))
    (loop :for i :below (array-total-size o)
          :do (setf (row-major-aref r i)
                    (row-major-aref o i)))
    r))

(defpolymorph-compiler-macro shallow-copy (array) (&whole form o &environment env)
  (with-type-info (_ (array-type &optional o-elt o-dim) env) o
    (when-types ((array-type array)) form
      (with-type-info (o-type () env) o
        `(the ,o-type
              ,(once-only (o)
                 `(let ((r (make-array ,(if (constant-array-dimensions-p o-dim env)
                                            `'(,@o-dim)
                                            `(array-dimensions ,o))
                                       :element-type ,(if (eql t o-elt) `(array-element-type ,o) `',o-elt)
                                       :adjustable t)))
                    (declare (type ,o-type ,o r))
                    (loop :for i :below (array-total-size ,o)
                       :do (setf (row-major-aref r i)
                                 (the ,o-elt
                                      (row-major-aref ,o i))))
                    r)))))))


;;Cons (and lists? unsure)
(defpolymorph shallow-copy ((o cons)) cons
  (cons (car o) (cdr o)))


;; Hash-table
(defpolymorph shallow-copy ((o hash-table)) (values hash-table &optional)
  (copy-hash-table o))


