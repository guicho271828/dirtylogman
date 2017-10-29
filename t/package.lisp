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
                            '("Solution found")))) ;note: without a !
  
  (is (equalp '(("found" . 0))
              (process-leaf 'exists
                            *logp*
                            nil
                            '("found")
                            '("Solution found"))))
  
  (is (equalp '(("expansion" . "17"))
              (process-leaf 'like
                            *logp*
                            nil
                            '("expansion")
                            '("Expanded 5 state(s)." "5"))))

  ;; experimental
  (is (equalp (dirtylogman::ensure-length
               100
               '(("landscape" . "19") ("landscape" . "18") ("landscape" . "16")
                 ("landscape" . "15") ("landscape" . "14") ("landscape" . "11")
                 ("landscape" . "10") ("landscape" . "9") ("landscape" . "8")
                 ("landscape" . "7") ("landscape" . "6") ("landscape" . "5")
                 ("landscape" . "4") ("landscape" . "3") ("landscape" . "2")
                 ("landscape" . "1") ("landscape" . "0"))
               '("landscape" . ""))
              (process-leaf 'collect-like
                            *logp*
                            nil
                            '("landscape")
                            '("100" "New best heuristic value for add(cost_type = one): 18" "18")))))

(test from-yaml
  ;; chekc the behavior of set-equal
  (is (alexandria:set-equal
       '(("time" . "0.00292444") ("solution" . 0) ("expansion" . "-1") ("solution-count" . "1"))
       '(("time" . "0.00292444") ("expansion" . "-1") ("solution" . 0) ("solution-count" . "1"))
       :test 'equal))
  
  (is ((lambda (a b) (alexandria:set-equal a b :test 'equal))
       '(("fig" . "fig2") ("mode" . "base") ("ipc" . "ipc2008") ("track" . "opt") ("domain" . "elevators-opt08")
         ("problem" . "p01") ("search" . "ad1e") ("timelimit" . "1800") ("memory" . "4000000")
         ("plan" . "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.plan.1")
         ("time" . "0.00292444")
         ("expansion" . "17")
         ("solution" . 0)
         ("count" . "1")
         ("numline" . "226")
         ("cost" . "66"))
       (let ((*default-pathname-defaults* (asdf:system-source-directory :dirtylogman)))
         ;; *default-pathname-defaults* is #p"" on CCL and absolute in SBCL. huh.
         (process (enough-namestring (asdf:system-relative-pathname :dirtylogman "sample.yaml")) *logp*)))))



