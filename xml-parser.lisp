;(ql:quickload :xmls)
(defpackage :swig-xml-processing
  (:use #:cl #:xmls #:cffi))

(in-package :swig-xml-processing)
;(setf xml (parse (uiop:read-file-string "C:/data/learn/swig/example_wrap.xml")))

;(defparameter *xml-file* "C:/data/learn/swig/example_wrap.xml")

(defun read-xml (xml-file)
  (parse (uiop:read-file-string xml-file)))


(defun extract-nodes (node name fn-collector)
  (if (string= name (node-name node))
      (funcall fn-collector node))
  (let ((children (node-children node)))
    (when children
      (mapc #'(lambda (cur-node)
                (extract-nodes cur-node name fn-collector))
            children))))

(defparameter *res* nil)
(defun collector (x)
  (push x *res*))

(defun get-cdecl-nodes (xml &aux cdecl collector)
  (setf collector (lambda (x) (push x cdecl)))
  (extract-nodes xml "cdecl" collector)
  (nreverse cdecl))

;(setf nodes (get-cdecl-nodes (read-xml *xml-file*)))

(defun get-attrs (node)
  (node-children (first (node-children node))))

(defun get-attr-value (node name)
  (second (assoc name (node-attrs node) :test 'string=)))
;(get-attr-value (sixth attrs) "name")

(defun is-function (attr-nodes)
  ;(setf attr-nodes (get-attrs (first nodes)))
  (loop for attr-node in attr-nodes
        if (and
            (string= "kind" (get-attr-value attr-node "name"))
            (string= "function" (get-attr-value attr-node "value")))
        do (return-from is-function t)))

;(is-function (get-attrs (second nodes)))

(defun get-only-func-nodes (nodes)
  (loop for node in nodes
        for attr-nodes = (get-attrs node)
        if (is-function attr-nodes)
        collect attr-nodes))

;(setf func-nodes (get-only-func-nodes nodes))


;(setf func-spec (first func-nodes))

(defstruct arg-spec
  name type)

(defun parm-to-arg-spec (parm-node &aux attrs)
  ;(setf parm-node (first (node-children attr-node)))
  (setf attrs (node-children (first (node-children parm-node))))
  (loop with arg-obj = (make-arg-spec)
        ;(setf arg-obj (make-arg-spec))
        for attr in attrs
        ;(setf attr (first attrs))
        for name = (get-attr-value attr "name")
        ;(setf name (get-attr-value attr "name"))
        do (cond
             ((string= "type" name)
              (setf (arg-spec-type arg-obj) (get-attr-value attr "value")))
             ((string= "name" name)
              (setf (arg-spec-name arg-obj) (get-attr-value attr "value"))))
        finally (return arg-obj)))
;(parm-to-arg-spec parm-node)

(defstruct fn-spec
  ret-type name args)


(defun get-fn-obj (func-spec)
  (loop
    with fn-obj = (make-fn-spec)
                                        ;(setf fn-obj (make-fn-spec))
    for attr-node in func-spec
                                        ;(setf attr-node (sixth func-spec))
    for node-name = (node-name attr-node)
                                        ;(setf node-name (node-name attr-node))
    for attr-name = (get-attr-value attr-node "name")
                                        ;(setf attr-name (get-attr-value attr-node "name"))
    do (cond
         ((and (string= "attribute" node-name) (string= "type" attr-name))
          (setf (fn-spec-ret-type fn-obj) (get-attr-value attr-node "value")))
         ((and (string= "attribute" node-name) (string= "name" attr-name))
          (setf (fn-spec-name fn-obj) (get-attr-value attr-node "value")))
         ((string= "parmlist" node-name)
          (setf (fn-spec-args fn-obj) (mapcar 'parm-to-arg-spec (node-children attr-node))))
         )
    finally (return fn-obj)))
;(setf fn-obj (get-fn-obj func-spec))

(defun create-arg-form (arg-obj &aux name arg-type)
  ;(setf arg-obj (first (fn-spec-args fn-obj)))
  (setf name (arg-spec-name arg-obj))
  (setf name (read-from-string (string-upcase name)))
  (setf arg-type (arg-spec-type arg-obj))
  (when (string= "char" arg-type)
    (setf arg-type "string"))
  (setf arg-type (read-from-string (uiop:strcat ":" (string-upcase arg-type))))
  `(,name ,arg-type))
;(create-arg-form arg-obj)

(defun create-func-form (fn-obj &aux ret-type name args lisp-name)
  (setf ret-type (fn-spec-ret-type fn-obj))
  (when (string= "char" ret-type)
    (setf ret-type "string"))
  (setf ret-type (read-from-string (uiop:strcat ":" (string-upcase ret-type))))
  (setf name (fn-spec-name fn-obj))
  (setf lisp-name (read-from-string (uiop:strcat "C-" (string-upcase name))))
  (setf args (mapcar 'create-arg-form (fn-spec-args fn-obj)))
  `(cffi:defcfun (,name ,lisp-name) ,ret-type
     ,@args))
;(create-func-form fn-obj)

(defun c-func-from-fn-obj (fn-obj &aux form)
  (setf form (create-func-form fn-obj))
  (print form)
  (eval form))
;(c-func-from-fn-obj fn-obj)
;(unintern 'c-fact)
;(c-fact 5)

(defun add-func-from-spec (func-spec)
  (c-func-from-fn-obj (get-fn-obj func-spec)))
;(add-func-from-spec func-spec)

(defun handling-xml (xml-file &aux func-nodes)
  ;(setf xml-file *xml-file*)
  (setf func-nodes (get-only-func-nodes (get-cdecl-nodes (read-xml xml-file))))
  (mapcar 'add-func-from-spec func-nodes))
;(setf syms (handling-xml *xml-file*))
;(c-fact 5)
;(c-my_mod 8 3)
;(c-get_time)

#|
(in-package :cffi)

(define-foreign-library _example
  (t (:default "C:/data/learn/swig/_example")))

(setf dll (use-foreign-library _example))

(defcfun ("fact" c-fact) :int
  (n :int))

(defcfun ("get_time" c-get_time) :string)
(c-get_time)
|#
;(c-fact 5)
