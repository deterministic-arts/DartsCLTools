#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Tools And Utilities
  Copyright (c) 2018, 2019, 2020 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:darts.lib.tools)

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
