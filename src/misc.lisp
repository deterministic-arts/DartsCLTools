#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Tools And Utilities
  Copyright (c) 2020 Dirk Esser

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
