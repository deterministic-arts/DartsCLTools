
(in-package "DARTS.LIB.TOOLS")

(defmacro label (name (&rest bindings) &body body)
  `(labels ((,name ,(mapcar #'first bindings) ,@body))
     (,name ,@(mapcar #'second bindings))))


(defmacro named-loop (loop-name (&rest bindings) &body body)
  (let ((restart (gensym "RESTART")))
    (multiple-value-bind (vars inits temps temps2)
        (loop
           for binding in bindings
           for var = (if (atom binding) binding (car binding))
           for init = (if (atom binding) 'nil (cadr binding))
           for temp = (gensym (string var))
           for temp2 = (gensym "TEMP")
           collecting var into all-vars
           collecting init into all-inits
           collecting temp into all-temps
           collecting temp2 into all-temps2
           finally (return (values all-vars all-inits all-temps all-temps2)))
      
      ;; This whole shuffling around of variables in two
      ;; "layers" is necessary due to the following edge
      ;; case:
      ;;
      ;;   (named-loop foo ((list '(1 2 3)))
      ;;     (let ((list list))
      ;;       (when list (foo (cdr list)))))
      ;;
      ;; If we'd simply re-assign naively, we'd assign to
      ;; the wrong binding in this case, causing this loop
      ;; to never terminate.      
      
      `(block ,loop-name
         (let (,@(mapcar (lambda (temp init) (list temp init)) temps inits)
               ,@vars)
           (macrolet ((,loop-name (,@temps2)
                        (list 'progn
                              (list 'psetq ,@(mapcan (lambda (t1 t2) (list `',t1 t2)) temps temps2))
                              (list 'go ',restart))))
             (tagbody
                ,restart
                (psetq ,@(mapcan (lambda (var temp) (list var temp)) vars temps))
                (return-from ,loop-name (progn ,@body)))))))))
