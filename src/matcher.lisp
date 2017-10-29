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

;;; split (wrapper over regex)

(defmethod process-leaf ((op (eql 'split)) input env variables rest)
  "Example:
[fig, mode, ipc, track, domain, problem, search, timelimit, memory]:
  - split fig - mode / ipc - track - * / domain / problem . search . timelimit . memory . * "
  (iter (for sub on rest)
        (ematch sub
          ((list* "*" next _)
           (collecting (format nil "[^~a]*" (char next 0)) into regex))
          
          ((list "*")
           nil)
          
          ((list* now next _)
           (if (member now variables :test 'equal)
               (progn
                 (assert (not (member next variables :test 'equal))
                         nil "Consecutive variables are not allowed")
                 (collecting (format nil "([^~a]*)" (char next 0)) into regex))
               (collecting (regex-escape now) into regex)))
          
          ((list now)
           (if (member now variables :test 'equal)
               (collecting "(.*)" into regex)
               (collecting (regex-escape now) into regex))))
        (finally
         (return
           (process-leaf 'regex input env variables (list (apply #'concatenate 'string regex)))))))

;;; shell
(defun ensure-length (n list &optional (default ""))
  (if (< (length list) n)
      (nconc list (make-list (- n (length list)) :initial-element default))
      list))
  
(defmethod process-leaf ((op (eql 'shell)) (input string) env variables commands)
  (nconc
   (mapcar #'cons variables
           (ensure-length
            (length variables)
            (shellwords:split
             (uiop:run-program commands
                               :input (make-string-input-stream input)
                               :output '(:string :stripped t)
                               :error-output t))))
   env))

(defmethod process-leaf ((op (eql 'shell)) (input pathname) env variables commands)
  (nconc
   (mapcar #'cons variables
           (ensure-length
            (length variables)
            (shellwords:split
             (uiop:run-program commands
                               :input input
                               :output '(:string :stripped t)
                               :error-output t))))
   env))

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
  (ematch rest
    ((list line)
     (process-leaf 'status input env variables `("grep" "-q" ,line)))))

;;; count

(defmethod process-leaf ((op (eql 'count)) (input string) env variables commands)
  (ematch commands
    ((list line)
     (nconc
      (mapcar #'cons variables
              (shellwords:split
               (uiop:run-program `("sh" "-c" ,(shellwords:join (list "grep" "-c" line)))
                                 :input (make-string-input-stream input)
                                 :output '(:string :stripped t)
                                 :error-output t
                                 :ignore-error-status t)))
      env))))

(defmethod process-leaf ((op (eql 'count)) (input pathname) env variables commands)
  (ematch commands
    ((list line)
     (nconc
      (mapcar #'cons variables
              (shellwords:split
               (uiop:run-program `("sh" "-c" ,(shellwords:join (list "grep" "-c" line)))
                                 :input input
                                 :output '(:string :stripped t)
                                 :error-output t
                                 :ignore-error-status t)))
      env))))

;;; like

(defmethod process-leaf ((op (eql 'like)) input env variables rest)
  (ematch rest
    ((list* line target options)
     (process-leaf 'shell input env variables
                   `("awk" ,(apply #'extract line target (mapcar #'read-from-string options)))))))

;;; default

(defmethod process-leaf ((op (eql 'default)) input env variables rest)
  (nconc (mapcar #'cons variables rest)
         env))

;;; collect-like
(defmethod process-leaf ((op (eql 'collect-like)) input env variables rest)
  (ematch* (rest variables)
    (((list* (read (and max (integer))) line target options)
      (list variable))
     (process-leaf 'shell input env (make-list max :initial-element variable)
                   `("awk" ,(apply #'extract-all line target (mapcar #'read-from-string options)))))))
