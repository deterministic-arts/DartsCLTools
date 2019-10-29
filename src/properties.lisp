#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Tools And Utilities
  Copyright (c) 2018 Dirk Esser

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

(in-package "DARTS.LIB.TOOLS")

(defgeneric property-list (object)
  (:documentation "Answers the property list of `object'. The
    result is a property list of th form
 
      (indicator1 value1 indicator2 value2 ...)

    i.e., a list, whose elements are pairs of keys and their 
    associated values. The keys are always compared using `eql'.
    The caller must never modify the return value of this function."))

(defgeneric update-property-list (object modifier)
  (:documentation "Modifies the property list of `object' by applying
    the function `modifier' to the current property list, and storing
    the result as the new property list. On implementations, which
    support it, this should be an atomic update.

    The `modifier' function is not allowed to modify the list 
    destructively.

    The modifier function returns two values: the new property list
    as first, and some other value as second one. This function returns
    the value returned by the modifier as second result."))



(defun property-value (object indicator &optional default)
  "Answers the value of the property named by `indicator' of the
   given `object'. If no matching property is found, answers `default'
   instead. This function returns as secondary value a boolean flag,
   which indicates, whether the property was found or not."
  (loop
     for (key value) on (property-list object) :by #'cddr
     if (eql key indicator) do (return (values value t))
     finally (return (values default nil))))


(defun copy-with-tail (list limit tail)
  (cond
    ((eq list limit) tail)
    ((eq (cdr list) limit) (cons (car list) tail))
    (t (let* ((first (cons (car list) nil))
              (last first))
         (loop
           for pair on (cdr list)
           unless (eq pair limit) 
             do (setf last (setf (cdr last) (cons (car pair) nil)))
           else
             do (setf (cdr last) tail)
                 (return first)
           finally (error "bogus call"))))))


(defun (setf property-value) (value object indicator &optional default)
  "Sets the value of the property named by `indicator' of the
   given `object' to `value'. The optional `default' is ignored
   and taken only for compatibility with `object-property'."
  (declare (ignore default))
  (labels
      ((modify (list)
         (loop
           for pair on list by #'cddr
           if (eql (car pair) indicator)
           do (return (list* indicator value (copy-with-tail list pair (cddr pair))))
           finally (return (list* indicator value list)))))
    (update-property-list object #'modify)
    value))

(defun ensure-property (object indicator constructor)
  (declare (dynamic-extent constructor))
  (let ((spot (update-property-list object
                (lambda (old-list)
                  (loop
                     for (key value) on old-list by #'cddr
                     when (eq key indicator)
                     do (return (values old-list (cons t value)))
                     finally (let ((value (funcall constructor)))
                               (return (values (list* indicator value old-list)
                                               (cons nil value)))))))))
    (values (cdr spot) (car spot))))

(defun map-over-properties (function object)
  "Applies `function' to each property of `object'. The function
   receives the property indicator as first, and the associated 
   values as second argument. The return value of the function
   is ignored. This function always returns `object'."
  (loop
    for (key value) on (property-list object) by #'cddr
    do (funcall function key value))
  object)

(defun remove-properties (object &optional (which 't))
  (labels
      ((modify (old-list)
         (loop
            for (key value) on old-list by #'cddr
            if (member key which :test #'eq)
              nconc (list key value) into removed
            else
              nconc (list key value) into new-list
            finally (if (null removed)
                        (return (values old-list nil))
                        (return (values new-list removed))))))
    (if (eq which 't)
        (update-property-list object (lambda (list) (values nil list)))
        (update-property-list object #'modify))))

(defun remove-property (object indicator)
  (let ((list (remove-properties object (list indicator))))
    (if list
        (values (second list) t)
        (values nil nil))))

(defun remove-properties-if (predicate object)
  (remove-properties-if-not (complement predicate) object))

(defun remove-properties-if-not (predicate object)
  (labels
      ((modify (old-list)
         (loop
            for (key value) on old-list by #'cddr
            if (funcall predicate key value)
              nconc (list key value) into new-list
            else
              nconc (list key value) into removed
            finally (if (null removed)
                        (return (values old-list nil))
                        (return (values new-list removed))))))
    (update-property-list object #'modify)))


(deftype plist-cell () '(cons null list))
  
(defmacro make-plist-cell (&optional inits)
  `(cons nil ,inits))
  
(defmacro plist-cell-read (cell-form)
  `(cdr ,cell-form))
  
(defmacro plist-cell-update (cell-form modifier-form)
  (let ((cell-val (gensym))
        (modifier (gensym))
        (new-value (gensym))
        (old-value (gensym))
        (result (gensym))
        (done (gensym)))
    `(let ((,cell-val ,cell-form)
           (,modifier ,modifier-form))
       (loop named ,done do
         (let ((,old-value (cdr ,cell-val)))
           (multiple-value-bind (,new-value ,result) (funcall ,modifier ,old-value)
             (when (or (eq ,old-value ,new-value)
                       (atomics:cas (cdr ,cell-val) ,old-value ,new-value))
               (return-from ,done ,result))))))))


(defclass property-support ()
  ((property-list :type plist-cell))
  (:documentation #.(concatenate 'string "Minimal mix-in class, which supports the object
    annotation protocol. By mixing this class into a class hierarchy,
    all instances gain support for function `property' and
    all related functions. The property list is (re-) initializable;
    note, however, that the initarg is named `property-list'
    as exported from this package, *not* by a keyword symbol.
    The property lists of instances of this class are upated 
    atomically.")))

(defmethod shared-initialize :after ((object property-support) slots &key ((property-list plist) nil got-plist))
  (declare (ignore slots))
  (when got-plist
    (setf (slot-value object 'property-list)
          (make-plist-cell (copy-list plist))))
  (unless (slot-boundp object 'property-list)
    (setf (slot-value object 'property-list)
          (make-plist-cell))))

(defmethod property-list ((object property-support))
  (plist-cell-read (slot-value object 'property-list)))

(defmethod update-property-list ((object property-support) modifier)
  (declare (dynamic-extent modifier))
  (plist-cell-update (slot-value object 'property-list) modifier))



#+SBCL
(defmacro define-structure-property-list (structure-type accessor)
  `(progn
     (defmethod property-list ((object ,structure-type))
       (,accessor object))
     (defmethod update-property-list ((object ,structure-type) modifier)
       (loop
          named done
          do (let ((old-list (,accessor object)))
               (multiple-value-bind (new-list result) (funcall modifier old-list)
                 (when (or (eq old-list new-list)
                           (atomics:cas (,accessor object) old-list new-list))
                   (return-from done result))))))))



(defmacro do-properties ((key value) object-form &body body)
  (let ((plist (gensym))
        (repeat (gensym)))
    `(block nil
       (let ((,plist (property-list ,object-form))
             ,key ,value)
         (tagbody
            ,repeat
            (unless ,plist (return))
            (setq ,key (pop ,plist))
            (setq ,value (pop ,plist))
            ,@body
            (go ,repeat))))))
