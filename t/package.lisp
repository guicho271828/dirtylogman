#|
  This file is a part of dirtylogman project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :dirtylogman.test
  (:use :cl
        :dirtylogman
        :fiveam
        :trivia :alexandria :iterate))
(in-package :dirtylogman.test)



(def-suite :dirtylogman)
(in-suite :dirtylogman)

;; run test with (run! test-name) 

(defvar *log* "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out")
(defvar *logp* (asdf:system-relative-pathname :dirtylogman (pathname *log*)))

(test process-leaf
  (is (equalp '(("fig" . "fig2") ("mode" . "base") ("ipc" . "ipc2008") ("track" . "opt") ("domain" . "elevators-opt08")
                ("problem" . "p01") ("search" . "ad1e") ("timelimit" . "1800") ("memory" . "4000000"))
              (process-leaf 'regex
                            *log*
                            nil
                            '("fig" "mode" "ipc" "track" "domain" "problem" "search" "timelimit" "memory")
                            '("([^-]*)-([^/]*)/([^-]*)-([^-]*)-[^/]*/([^/]*)/([^.]*)\.([^.]*)\.([^.]*)\.([^.]*)\.out"))))
  
  (is (equalp '(("fig" . "fig2") ("mode" . "base") ("ipc" . "ipc2008") ("track" . "opt") ("domain" . "elevators-opt08")
                ("problem" . "p01") ("search" . "ad1e") ("timelimit" . "1800") ("memory" . "4000000"))
              (process-leaf 'split
                            *log*
                            nil
                            '("fig" "mode" "ipc" "track" "domain" "problem" "search" "timelimit" "memory")
                            '("fig" "-" "mode" "/" "ipc" "-" "track" "-" "*" "/" "domain" "/" "problem" "." "search" "." "timelimit" "." "memory"
                              "." "*"))))

  (is (equalp '(("a" . "aaa") ("b" . "bbb") ("c" . "ccc"))

              (process-leaf 'split
                            "aaa bbb ccc"
                            nil
                            '("a" "b" "c")
                            '("a" " " "b" " " "c"))))

  (is (equalp '(("err" . "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.err"))
              (process-leaf 'shell
                            *log*
                            nil
                            '("err")
                            '("sed" "s/out/err/g"))))

  (is (equalp '(("numline" . "226"))
              (process-leaf 'shell
                            *logp*
                            nil
                            '("numline")
                            '("wc" "-l"))))

  (is (equalp '(("a" . "1") ("b" . "2"))
              (process-leaf 'shell "1 2" nil '("a" "b") '("cat"))))

  (is (equalp '(("a" . "1") ("b" . "2"))
              (process-leaf 'shell (format nil "1~%2") nil '("a" "b") '("cat"))))
  
  (is (equalp '(("true" . 0))
              (process-leaf 'status "" nil '("true") '("true"))))
  
  (is (equalp '(("false" . 1))
              (process-leaf 'status "" nil '("false") '("false"))))
  
  (is (equalp '(("count" . "2"))
              (process-leaf 'count
                            *logp*
                            nil
                            '("count")
                            '("Solution found"))))
  
  (is (equalp '(("found" . 0))
              (process-leaf 'exists
                            *logp*
                            nil
                            '("found")
                            '("Solution found"))))
  
  (is (equalp '(("expansion" . "17"))
              (process-leaf 'like
                            
                            nil
                            '("expansion")
                            '("Expanded 5 state(s)." "5")))))

(test from-yaml
  (is (equalp '(("time" . "0.00292444") ("expansion" . "-1") ("solution" . 0) ("solution-count" . "1")
                ("numline" . "226") ("time" . "0.00292444") ("expansion" . "-1") ("solution" . 0)
                ("solution-count" . "1") ("numline" . "226") ("time" . "0.00292444") ("expansion" . "-1")
                ("solution" . 0) ("solution-count" . "1") ("numline" . "226")
                ("err"
                 . "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out")
                ("plan"
                 . "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out"))
              (process (asdf:system-relative-pathname :dirtylogman "sample.yaml") *logp*))))



