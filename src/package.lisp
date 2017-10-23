#|
  This file is a part of dirtylogman project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage dirtylogman
  (:use :cl :trivia :alexandria :iterate
        :arrow-macros)
  (:shadowing-import-from :arrow-macros :<>)
  (:export
   #:process-leaf
   #:regex
   #:split
   #:shell
   #:count
   #:like
   #:status
   #:exists
   #:process))
(in-package :dirtylogman)

;; blah blah blah.

