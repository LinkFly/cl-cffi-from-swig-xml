(in-package :cl-user)
(require 'asdf)

;(setf *load-pathname* #P"c:/data/learn/swig/test.lisp")
(defparameter *this-dir* (make-pathname :name nil :type nil :defaults *load-pathname*))
(defparameter *config* (uiop:read-file-form (truename (merge-pathnames "config.sexp" cl-user::*this-dir*))))
(defparameter *ql-setup* (truename (getf *config* 'quicklisp-setup)))

(load *ql-setup*)
(ql:quickload '(:xmls :cffi))
(load (truename (merge-pathnames "xml-parser.lisp" *this-dir*)))

(in-package :swig-xml-processing)

(defparameter *xml-filename* "example_wrap.xml")
(defparameter *xml-filepath* (truename (merge-pathnames *xml-filename* cl-user::*this-dir*)))

(define-foreign-library _example
  (t (:default "C:/data/learn/swig/_example-1")))

(defparameter *example-dll* (use-foreign-library _example))

(let (syms)
  (setf syms (handling-xml *xml-filepath*))
  (format t "~%Defined syms: ~A" syms))


(let (fact (wait 120))
  (setf fact (c-fact 5))
  (assert (= wait fact) (wait fact) "TEST FAILED - c-fact. Wait: ~a, Fact: ~a" wait fact))

(let (fact (wait 2))
  (setf fact (c-my_mod 8 3))
  (assert (= wait fact) (wait fact) "TEST FAILED - c-my_mod. Wait: ~a, Fact: ~a" wait fact))

(let (fact (wait '(> 0)))
  (setf fact (if (> (length (c-get_time)) 0) '(> 0) '(not (> 0))))
  (assert (equal wait fact) (wait fact) "TEST FAILED - c-get_time(length_of_return). Wait: ~a, Fact: ~a" wait fact))

(format t "~2%TESTS PASSED")

(cl-user::quit)
