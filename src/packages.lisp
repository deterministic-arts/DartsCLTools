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
  
(defpackage #:darts.lib.tools.properties
  (:use)
  (:export #:property-list #:update-property-list #:property-value
           #:remove-properties #:ensure-property #:property-support
           #:define-structure-property-list #:map-over-properties
           #:do-properties #:remove-properties-if #:remove-properties-if-not
           #:remove-property #:delete-property #:delete-properties #:delete-properties-if
           #:delete-properties-if-not #:update-property-value))

(defpackage #:darts.lib.tools.observables
  (:use)
  (:export #:observable #:add-observer #:remove-observer #:observe-event
           #:notify-observers #:add-observer-to-chain #:remove-observer-from-chain
           #:do-observers-in-chain #:notify-observers-in-chain #:parent-observer-chain
           #:make-observer-chain #:observer-chain-entries #:observer-chain-next
           #:observer-chain-p))

(defpackage #:darts.lib.tools
  (:use #:common-lisp #:darts.lib.tools.properties #:darts.lib.tools.observables)
  (:export #:property-list #:update-property-list #:property-value
           #:remove-properties #:ensure-property #:property-support
           #:define-structure-property-list #:map-over-properties
           #:do-properties #:remove-properties-if #:remove-properties-if-not
           #:remove-property #:delete-property #:delete-properties #:delete-properties-if
           #:delete-properties-if-not #:update-property-value

           #:named-loop #:label #:preserving-evaluation-order
     
           #:observable #:add-observer #:remove-observer #:observe-event
           #:notify-observers #:add-observer-to-chain #:remove-observer-from-chain
           #:do-observers-in-chain #:notify-observers-in-chain #:parent-observer-chain
           #:make-observer-chain #:observer-chain-entries #:observer-chain-next
           #:observer-chain-p))
