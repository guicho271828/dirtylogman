
(in-package :dirtylogman)

(defun gget (hash key)
  "generic hash table access"
  (etypecase hash
    (hash-table (gethash key hash))
    (list
     (if (consp (first hash))
         (cdr (assoc key hash :test 'equal))
         (getf hash key)))))


;;; sample yaml parse input

(defun alist (yaml)
  (match yaml
    ((hash-table)
     (alist (hash-table-alist yaml)))
    ((cons car cdr)
     (cons (alist car)
           (alist cdr)))
    (_ yaml)))

(defun read-yaml (pathname)
  (alist
   (yaml:parse (read-file-into-string pathname)
               :multi-document-p t)))

;; (read-yaml "sample.yaml")

#+(or)
(("numline" "shell \"wc -l\"") ("solution-count" "count \"Solution found!\"")
 ("solution" "exists \"Solution found!\"") ("expansion" "like \"Expanded 5 state(s).\" \"5\" :mode :around")
 ("time" "like \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\"")
 ("path" "concatenate _ \"/\" noise \"/\" _ \".\" _ \".\" track \"/\" steps \"-\" no \"-\" _ \"/\" _"))

;;; 

(defun process (yaml input)
  (let ((*package* (find-package :dirtylogman)))
    (ematch (read-yaml yaml)
      ((list* :documents primary secondaries)
       (->> (process-pathname primary input)
         (process-primary primary input)
         (process-secondaries primary secondaries))))))

(defun process-pathname (primary input)
  (let ((rule (gget primary "pathname")))
    (process-tree rule (enough-namestring input) nil)))

(defun process-primary (primary input env)
  (process-tree primary input env))

(defun process-secondaries (primary secondaries env)
  (iter (for rule in secondaries)
        (for key in (gget primary "secondary"))
        (setf env
              (process-tree rule (pathname (gget env key)) env)))
  env)

(defun process-tree (rule input env)
  (ematch rule
    ((list* (cons (or "pathname" "secondary") _) rest) ;special keywords
     (process-tree rest input env))
    ((list* (cons key matchers) rest)
     (process-tree
      rest input
      (iter (for matcher in matchers)
            (thereis
             (ematch (shellwords:split matcher)
               ((list* (read op) args)
                (process-leaf op
                              input
                              env 
                              (ensure-list key)
                              args)))))))
    (nil
     env)))

              
