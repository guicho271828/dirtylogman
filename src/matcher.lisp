;;;; process-leaf operations
(in-package :dirtylogman)

(defgeneric process-leaf (op input env variables rest))

;;; regex

(defmethod process-leaf :around ((op (eql 'regex)) (input pathname) env variables rest)
  (apply #'process-leaf op (read-file-into-string input) env variables rest))


(defmethod process-leaf ((op (eql 'regex)) input env variables rest)
  (ematch rest
    ((list regex)
     (nconc (map 'list #'cons
                 variables
                 (nth-value 1 (ppcre:scan-to-strings regex input)))
            env))))

;; [fig, mode, ipc, track, domain, problem, search, timelimit, memory]:
;;   - regex "([^-]*)-([^/]*)/([^-]*)-([^-]*)-[^/]*/([^/])*/([^.])*\.[^.]*\.([^.])*\.([^.])*\.out"
#+(or)
(process-leaf 'regex
              "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out"
              nil
              '("fig" "mode" "ipc" "track" "domain" "problem" "search" "timelimit" "memory")
              '("([^-]*)-([^/]*)/([^-]*)-([^-]*)-[^/]*/([^/]*)/([^.]*)\.([^.]*)\.([^.]*)\.([^.]*)\.out"))

#+(or)
(("fig" . "fig2") ("mode" . "base") ("ipc" . "ipc2008") ("track" . "opt") ("domain" . "elevators-opt08")
 ("problem" . "p01") ("search" . "ad1e") ("timelimit" . "1800") ("memory" . "4000000"))

;;; split (wrapper over regex)

(defmethod process-leaf ((op (eql 'split)) input env variables rest)
  "Example:
[fig, mode, ipc, track, domain, problem, search, timelimit, memory]:
  - split fig - mode / ipc - track - * / domain / problem . search . timelimit . memory . * "
  (iter (for sub on rest)
        (ematch sub
          ((list* "*" next _)
           (collecting (format nil "[^~a]*" (char next 0)) into regex))
          ((list* now next _)
           (if (member now variables :test 'equal)
               (progn
                 (assert (not (member next variables :test 'equal))
                         nil "Consecutive variables are not allowed")
                 (collecting (format nil "([^~a]*)" (char next 0)) into regex))
               (collecting now into regex)))
          ((list now)
           (if (member now variables :test 'equal)
               (collecting "(.*)" into regex)
               (collecting now into regex))))
        (finally
         (return
           (process-leaf 'regex input env variables (list (apply #'concatenate 'string regex)))))))

#+(or)
(process-leaf 'split
              "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out"
              nil
              '("fig" "mode" "ipc" "track" "domain" "problem" "search" "timelimit" "memory")
              '("fig" "-" "mode" "/" "ipc" "-" "track" "-" "*" "/" "domain" "/" "problem" "." "search" "." "timelimit" "." "memory"
 "." "*"))

#+(or)
(("fig" . "fig2") ("mode" . "base") ("ipc" . "ipc2008") ("track" . "opt") ("domain" . "elevators-opt08")
 ("problem" . "p01") ("search" . "ad1e") ("timelimit" . "1800") ("memory" . "4000000"))

#+(or)
(process-leaf 'split
              "aaa bbb ccc"
              nil
              '("a" "b" "c")
              '("a" " " "b" " " "c"))

#+(or)
(("a" . "aaa") ("b" . "bbb") ("c" . "ccc"))

;;; shell

(defmethod process-leaf ((op (eql 'shell)) (input string) env variables commands)
  (nconc
   (mapcar #'cons variables
           (shellwords:split
            (uiop:run-program commands
                              :input (make-string-input-stream input)
                              :output '(:string :stripped t)
                              :error-output t
                              :ignore-error-status t)))
   env))

(defmethod process-leaf ((op (eql 'shell)) (input pathname) env variables commands)
  (nconc
   (mapcar #'cons variables
           (shellwords:split
            (uiop:run-program commands
                              :input input
                              :output '(:string :stripped t)
                              :error-output t
                              :ignore-error-status t)))
   env))

;; pathname:
;;   err:
;;     - shell sed s/log/err/g

#+(or)
(process-leaf 'shell
              "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out"
              nil
              '("err")
              '("sed" "s/out/err/g"))

#+(or)
(("err" . "fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.err"))

;; on file
;; numline:
;;   - shell wc -l

#+(or)
(process-leaf 'shell
              #p"fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55/elevators-opt08/p01.ad1e.1800.4000000.out"
              nil
              '("numline")
              '("wc" "-l"))

#+(or)
(("numline" . "226"))

;;; status

(defmethod process-leaf ((op (eql 'status)) (input string) env variables commands)
  (nconc
   (mapcar #'cons variables
           (list
            (nth-value
             2
             (uiop:run-program commands
                               :input (make-string-input-stream input)
                               :output nil
                               :error-output nil
                               :ignore-error-status t))))
   env))

(defmethod process-leaf ((op (eql 'status)) (input pathname) env variables commands)
  (nconc
   (mapcar #'cons variables
           (list
            (nth-value
             2
             (uiop:run-program commands
                               :input input
                               :output nil
                               :error-output nil
                               :ignore-error-status t))))
   env))

;;; exists

(defmethod process-leaf ((op (eql 'exists)) input env variables rest)
  (match rest
    ((list line)
     (process-leaf 'status input env variables `("grep" "-q" ,line)))))

;;; count

(defmethod process-leaf ((op (eql 'count)) input env variables rest)
  (match rest
    ((list line)
     (process-leaf 'shell input env variables
                   `("sh" "-c" ,(shellwords:join (list "grep" "-c" line)))))))

;;; like

(defmethod process-leaf ((op (eql 'like)) input env variables rest)
  (match rest
    ((list* line target options)
     (process-leaf 'shell input env variables
                   `("awk" ,(apply #'extract line target (mapcar #'read-from-string options)))))))



