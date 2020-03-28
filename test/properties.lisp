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

(in-package #:darts.lib.tools.test)
(in-suite properties-suite)



;;; The following tests make sure, that all property-related functions
;;; exported by the library work as advertised, if the object holding
;;; the property list properly supports the generic functions PROPERTY-LIST
;;; and UPDATE-PROPERTY-LIST.
;;;
;;; We don't event care for thread-safety or atomic access here. The
;;; protocol was designed such, that some form of thread-safety is possible,
;;; but it is not a requirement.

(defconstant +singleton+ '+singleton+)
(defvar *plist*)

(defmethod property-list ((object (eql +singleton+)))
  (declare (ignore object))
  *plist*)

(defmethod update-property-list ((object (eql +singleton+)) modifier)
  (declare (ignore object))
  (multiple-value-bind (new-value result) (funcall modifier *plist*)
    (setf *plist* new-value)
    result))


(defun plist-equal (pl1 pl2 &key (test #'eql))
  (let ((missing (cons nil nil)))
    (and (eql (length pl1) (length pl2))
         (loop
            for (k1 v1) on pl1 by #'cddr
            as v2 = (getf pl2 k1 missing)
            unless (funcall test v1 v2)
            do (return nil)
            finally (return t)))))
                                   

(test reading-properties
  (let ((*plist* (list 'a 1 'b 2)))
    (multiple-value-bind (value found) (property-value +singleton+ 'b)
      (is-true found)
      (is (eql 2 value)))
    (multiple-value-bind (value found) (property-value +singleton+ 'c 3)
      (is-false found)
      (is (eql 3 value)))))
    
(test writing-properties
  (let ((*plist* (list 'a 1 'b 2)))
    (setf (property-value +singleton+ 'a) 3)
    (multiple-value-bind (value found) (property-value +singleton+ 'a)
      (is-true found)
      (is (eql 3 value)))
    (multiple-value-bind (value found) (property-value +singleton+ 'b)
      (is-true found)
      (is (eql 2 value))))
  (let ((*plist* (list 'a 1 'b 2)))
    (setf (property-value +singleton+ 'b) 3)
    (multiple-value-bind (value found) (property-value +singleton+ 'a)
      (is-true found)
      (is (eql 1 value)))
    (multiple-value-bind (value found) (property-value +singleton+ 'b)
      (is-true found)
      (is (eql 3 value)))))

(test deleting-single-properties
  (let ((data '((a 1 t) (b 2 t) (c 3 t) (d nil nil))))
    (loop
       for (key value expected) in data
       do (let ((*plist* (list 'a 1 'b 2 'c 3)))
            (multiple-value-bind (old-value found) (delete-property +singleton+ key)
              (when expected
                (is-true found "key ~S should have been present with value ~S" key value)
                (is (eql value old-value))
                (multiple-value-bind (value found) (property-value +singleton+ key +singleton+)
                  (is (eq +singleton+ value) "key ~S should have been gone afterwards, but we still found ~S" key value)
                  (is-false found))
                (loop
                   for (k v e) in data
                   when (and e (not (eq k key)))
                   do (multiple-value-bind (value found) (property-value +singleton+ k)
                        (is (eql v value) "deletion should not have touched key ~S" k)
                        (is-true found))))
              (unless expected
                (is-false found)
                (is (null old-value))))))))

(test deleting-properties
  (let ((plist '(a 1 b 2 c 3))
        (data '(((a) (b 2 c 3) (a 1))
                ((a b) (c 3) (a 1 b 2))
                ((a b c) () (a 1 b 2 c 3))
                ((d) (a 1 b 2 c 3) ())
                ((b) (a 1 c 3) (b 2))
                ((b c) (a 1) (b 2 c 3)))))
    (loop
       for (keys expected-rest expected-result) in data
       do (let* ((*plist* plist)
                 (result (delete-properties +singleton+ keys)))
            (is (plist-equal expected-result result))
            (is (plist-equal expected-rest (property-list +singleton+)))))))
            
