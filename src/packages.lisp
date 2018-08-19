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

(defpackage "DARTS.LIB.TOOLS"
  (:use "COMMON-LISP")
  (:export
     "PROPERTY-LIST" "UPDATE-PROPERTY-LIST" "PROPERTY-VALUE"
     "REMOVE-PROPERTIES" "ENSURE-PROPERTY" "PROPERTY-SUPPORT"
     "DEFINE-STRUCTURE-PROPERTY-LIST" "MAP-OVER-PROPERTIES"
     "DO-PROPERTIES" "FILTER-PROPERTIES" "REMOVE-PROPERTY"

     "NAMED-LOOP" "LABEL"
     
     "OBSERVABLE" "ADD-OBSERVER" "REMOVE-OBSERVER" "OBSERVE-EVENT"
     "NOTIFY-OBSERVERS" "ADD-OBSERVER-TO-CHAIN" "REMOVE-OBSERVER-FROM-CHAIN"
     "NOTIFYING-OBSERVERS-IN-CHAIN" "NOTIFY-OBSERVERS-IN-CHAIN"))
