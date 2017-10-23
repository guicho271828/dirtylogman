
(in-package :dirtylogman)

(defun gget (hash key)
  "generic hash table access"
  (etypecase hash
    (hash-table (gethash key hash))
    (list
     (if (consp (first hash))
         (cdr (assoc key hash))
         (getf hash key)))))


;;; sample yaml parse input

(defun read-yaml (pathname)
  (yaml:parse (read-file-into-string pathname)
              :multi-document-p t))

;; hashtable by default

#+(or)
(("numline" "shell \"wc -l\"") ("solution-count" "count \"Solution found!\"")
 ("solution" "exists \"Solution found!\"") ("expansion" "like \"Expanded 5 state(s).\" \"5\" :mode :around")
 ("time" "like \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\"")
 ("path" "concatenate _ \"/\" noise \"/\" _ \".\" _ \".\" track \"/\" steps \"-\" no \"-\" _ \"/\" _"))

(defun process (yaml input)
  (ematch (read-yaml yaml)
    ((list* :documents primary secondaries)
     (-<> (process-pathname primary input)
       (process-primary primary input)
       (process-secondaries primary secondaries)))))

(defun process-pathname (primary input)
  (let ((rule (gethash "pathname" primary)))
    (run-match (enough-namestring input) rule nil)))

(defun process-primary (primary input env)
  (run-match input primary env))

(defun process-secondaries (primary secondaries env)
  (iter (for rule in secondaries)
        (for key in (gethash "secondary" primary))
        (setf env
              (run-match (pathname (gget env key))
                         primary env))))

(defun run-match (input rule env)
  (iter (for (key matchers) in-hashtable rule)

        (when (equal "pathname" key)
          (next-iteration))
        
        (setf env
              (iter (for matcher in matchers)
                    (thereis
                     (with-input-from-string (s matcher)
                       (apply (read s)
                              input
                              env 
                              (ensure-list key)
                              (iter (for o in-stream s)
                                    (collect o)))))))))

              
