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
(in-suite observable-suite)

(test adding-to-an-observer-chain
  (let ((chain (make-observer-chain))
        (observer1 (make-symbol "OBSERVER"))
        (observer2 (make-symbol "OBSERVER")))
    (multiple-value-bind (object added)
        (add-observer-to-chain observer1 chain :test #'string= :key #'symbol-name)
      (is (eq observer1 object))
      (is-true added))
    (multiple-value-bind (object added)
        (add-observer-to-chain observer2 chain :test #'string= :key #'symbol-name)
      (is (eq observer1 object))
      (is-false added))))

(test removing-from-an-observer-chain
  (let ((chain (make-observer-chain))
        (observer1 (make-symbol "OBSERVER"))
        (observer2 (make-symbol "OBSERVER")))
    (multiple-value-bind (object added)
        (add-observer-to-chain observer1 chain :test #'string= :key #'symbol-name)
      (is (eq observer1 object))
      (is-true added))
    (multiple-value-bind (object removed)
        (remove-observer-from-chain observer2 chain :test #'string= :key #'symbol-name)
      (is (eq observer1 object))
      (is-true removed))))

(test do-observers-on-empty-chain-has-no-side-effects
  (let ((chain (make-observer-chain (make-observer-chain (make-observer-chain))))
        (effects nil)
        (count 0))
    (do-observers-in-chain (var (effect (incf count))) chain
      (push (cons var effect) effects))
    (is (null effects))
    (is (eql 0 count))))

(test do-observers-on-non-empty-chain-evals-args-once
  (let* ((root (make-observer-chain))
         (chain (make-observer-chain root))
         (effects nil)
         (count 0))
    (add-observer-to-chain 'outer root)
    (add-observer-to-chain 'inner chain)
    (do-observers-in-chain (var (effect (incf count))) chain
      (push (cons var effect) effects))
    ;; The order here is well-defined: the observers in CHAIN must be
    ;; seen before the observers in ROOT, and hence be pushed earlier
    ;; onto the list. If we had more than one observer on either CHAIN
    ;; or ROOT, their relative ordering would be undefined, though.
    (is (equal '((outer . 1) (inner . 1)) effects))
    (is (eql 1 count))))
