#|
  This file is a part of dirtylogman project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Tools for reading lots of log files

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage dirtylogman-asd
  (:use :cl :asdf))
(in-package :dirtylogman-asd)


(defsystem dirtylogman
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate :trivia.ppcre :ppcre)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Tools for reading lots of log files"
  :in-order-to ((test-op (test-op :dirtylogman.test))))
