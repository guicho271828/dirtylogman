#|
  This file is a part of dirtylogman project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :dirtylogman.test
  (:use :cl
        :dirtylogman
        :fiveam
        :trivia :alexandria :iterate :trivia.ppcre))
(in-package :dirtylogman.test)



(def-suite :dirtylogman)
(in-suite :dirtylogman)

;; run test with (run! test-name) 

(test dirtylogman

  )



