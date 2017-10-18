#|
  This file is a part of dirtylogman project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage dirtylogman.test-asd
  (:use :cl :asdf))
(in-package :dirtylogman.test-asd)


(defsystem dirtylogman.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dirtylogman"
  :license "LLGPL"
  :depends-on (:dirtylogman
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(5am:run! :dirtylogman)"))))
