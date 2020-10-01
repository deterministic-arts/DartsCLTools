
(in-package #:darts.lib.tools)

(defmacro preserving-evaluation-order ((name) &body body)
  (let ((bindings-var (gensym "DEFERRED"))
        (result-var (gensym "RESULT"))
        (form-var (gensym "FORM"))
        (temp-var (gensym)))
    `(let (,bindings-var)
       (let ((,result-var (flet ((,name (,form-var)
                                   (let ((,temp-var (gensym)))
                                     (push (list ,temp-var ,form-var) ,bindings-var)
                                     ,temp-var)))
                            ,@body)))
         (if (not ,bindings-var) ,result-var
             `(let (,@(reverse ,bindings-var))
                ,,result-var))))))
