
(in-package :dirtylogman)

(defgeneric process-leaf (op input env variables &rest rest))

(defmethod process-leaf :around (op (input pathname) env variables &rest rest)
  (apply #'process-leaf op (read-file-into-string input) env variables rest))


(defmethod process-leaf ((op (eql 'regex)) input env variables &rest rest)
  (ematch rest
    ((list regex)
     (nconc (map 'list #'cons
                 variables
                 (ppcre:scan-to-strings regex input))
            env))))

;; (defmethod process-leaf ((op (eql 'split)) input env variables &rest rest)
;;   (iter (for arg in rest)
;;         
;;   (ematch rest
;;     ((list regex)
;;      (nconc (map 'list #'cons
;;                  variables
;;                  (ppcre:scan-to-strings regex input))
;;             env))))


