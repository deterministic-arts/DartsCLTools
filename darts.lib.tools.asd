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

(asdf:defsystem #:darts.lib.tools
  :name "darts.lib.tools"
  :author "Dirk Esser"
  :version "0.1"
  :maintainer "Dirk Esser"
  :licence "MIT"
  :description "More or less useful utilities"
  :long-description ""
  :in-order-to ((asdf:test-op (asdf:test-op "darts.lib.tools.test")))
  :depends-on (#:atomics)
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:file "properties" :depends-on ("packages"))
     (:file "iteration" :depends-on ("packages"))
     (:file "observable" :depends-on ("packages"))
     (:file "misc" :depends-on ("packages"))))))
