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

(defgeneric observe-event (observer object event)
  (:argument-precedence-order observer object event)
  (:documentation "Invoked in order to notify `observer`, that the event
    described by `event` has happened with respect to `object`."))

(defgeneric add-observer (observer object &key test key identity)
  (:argument-precedence-order object observer)
  (:documentation "Adds the given `observer` to the set of registered 
    event observers of `object`, unless it is already present."))

(defgeneric remove-observer (observer object &key test key)
  (:argument-precedence-order object observer)
  (:documentation "Removes the given `observer` from the set of registered
    event observers of `object`"))

(defgeneric notify-observers (object event)
  (:argument-precedence-order object event)
  (:documentation "Notifies all registered event observers of `object`
    about the occurence of `event`."))

(defgeneric parent-observer-chain (object)
  (:method (object) (declare (ignore object)) nil)
  (:documentation ""))



;;; The following definitions represent a variation of the theme on a
;;; lower level than the API functions declared above. Incidentally, we
;;; can also use these functions in the implementations of the higher
;;; level mechanism, that we provide below.

(atomics:defstruct (observer-chain (:copier nil)
                                   (:constructor make-observer-chain (&optional next))
                                   (:predicate observer-chain-p))
  (entries nil)
  (next nil :type (or null observer-chain) :read-only t))

(defmethod print-object ((object observer-chain) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "SIZE=~D~@[ PARENT=~S~]" (length (observer-chain-entries object)) (observer-chain-next object)))
  object)

(macrolet
    ((update-place (place old-form new-form)
       (let ((o (gensym))
             (n (gensym)))
         `(let ((,o ,old-form) (,n ,new-form))
            (or (eq ,o ,n)
                (atomics:cas ,place ,o ,n))))))

(defun add-observer-to-chain (observer chain &key (test #'eql) (key #'identity) (identity (funcall key observer)))
  (loop
    (let* ((old-list (observer-chain-entries chain))
           (present (member identity old-list :key key :test test)))
      (if present
          (return-from add-observer-to-chain (values (car present) nil))
          (let ((new-list (cons observer old-list)))
            (when (update-place (observer-chain-entries chain) old-list new-list)
              (return-from add-observer-to-chain (values observer t))))))))

(defun remove-observer-from-chain (observer chain &key (test #'eql) (key #'identity))
  (macrolet ((test (v1 v2) `(funcall test ,v1 ,v2))
             (key (value) `(funcall key ,value)))
    (loop
       (let ((old-list (observer-chain-entries chain)))
         (multiple-value-bind (new-list old-observer found)
             (cond
               ((null old-list) (values old-list nil nil))
               ((test observer (key (car old-list))) (values (cdr old-list) (car old-list) t))
               (t (let* ((head (cons (car old-list) nil))
                         (tail head))
                    (loop
                       for link on (cdr old-list)
                       for elt = (car link)
                       unless (test observer (key elt))
                         do (setf tail (setf (cdr tail) (cons elt nil)))
                       else
                         do (setf (cdr tail) (cdr link))
                            (return (values head elt t))
                       finally (return (values old-list nil nil))))))
           (when (or (not found) (update-place (observer-chain-entries chain) old-list new-list))
             (return-from remove-observer-from-chain (values old-observer found))))))))

nil)                                    ; macrolet

(defmacro do-observers-in-chain ((observer-var &rest aux-bindings) chain-form &body body)
  (let ((spine (gensym "CHAIN"))
        (inner (gensym "INNER"))
        (skip (gensym "SKIP"))
        (advance (gensym "ADVANCE"))
        (done (gensym "DONE"))
        (invoke (gensym "INVOKE")))
    `(let ((,spine ,chain-form)
           ,inner)
       (tagbody
          ,skip
          (unless ,spine (go ,done))
          (multiple-value-setq (,inner ,spine)
            (values (observer-chain-entries ,spine)
                    (observer-chain-next ,spine)))
          (unless ,inner (go ,skip))
          (let* (,@aux-bindings)
            (tagbody
               ,invoke
               (let ((,observer-var (pop ,inner))) ,@body)
               (when ,inner (go ,invoke))
               ,advance
               (unless ,spine (go ,done))
               (multiple-value-setq (,inner ,spine)
                 (values (observer-chain-entries ,spine)
                         (observer-chain-next ,spine)))
               (unless ,inner (go ,advance))
               (go ,invoke)))
          ,done))))

(defun notify-observers-in-chain (chain object event)
  (loop
     for link = chain then (observer-chain-next link) while link
     do (dolist (observer (observer-chain-entries link))
          (observe-event observer object event))))



(defclass observable ()
  ((observer-list-chain)))

(defmethod shared-initialize :after ((object observable) slots &key)
  (declare (ignore slots))
  (setf (slot-value object 'observer-list-chain)
        (make-observer-chain (parent-observer-chain object))))

(defmethod add-observer (observer (object observable)
                         &key (test #'eql) (key #'identity) (identity (funcall key observer)))
  (with-slots (observer-list-chain) object
    (add-observer-to-chain observer observer-list-chain
                           :test test :key key
                           :identity identity)))

(defmethod remove-observer (observer (object observable)
                            &key (test #'eql) (key #'identity))
  (with-slots (observer-list-chain) object
    (remove-observer-from-chain observer observer-list-chain
                                :test test :key key)))

(defmethod notify-observers ((object observable) event)
  (with-slots (observer-list-chain) object
    (loop
       for link = observer-list-chain then (observer-chain-next link) while link
       do (dolist (ob (observer-chain-entries link))
            (observe-event ob object event)))))

