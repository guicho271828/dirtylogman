
(in-package :dirtylogman)

(defun sh (command &key (shell "sh"))
  "Wrapper for uiop:run-program"
  (uiop:run-program `(,shell "-c" ,command)
                    :output '(:string :stripped t)
                    :error-output t
                    :ignore-error-status t))

(defun grep (&rest args)
  "Wrapper for grep"
  (sh (format nil "grep ~{~a~^ ~}" args)))

(defun awk (&rest args)
  "Wrapper for awk"
  (sh (format nil "awk ~{'~a'~^ ~}" args)))

(defun squote (str)
  "Shell-Quote: Quote the string with ''"
  (format nil "'~a'" str))

(defun extract (line target &key (mode :before))
  "Returns an awk script as a string which extracts TARGET from LINE.
MODE specifies which context it should consider.

 (extract \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\") -> 

\"BEGINFILE{s=-1}ENDFILE{print s}/^[[:space:]]*Actual[[:space:]]+search[[:space:]]+time:/{s=$4}\"

"
  (awk
   (flet ((escape (s)
            (coerce (iter (for c in-string s)
                          (if (find c "()[]{}?.+*\\^$/|")
                              (appending (list #\\ c))
                              (collect c)))
                    'string)))
     (let* ((line (escape line))
            (target (escape target))
            (tokens (split-sequence #\Space line :remove-empty-subseqs t))
            (p (position target tokens :test 'equal))
            (common "BEGINFILE{s=-1}ENDFILE{print s}"))
       (assert (member target tokens :test 'equal))
       (ecase mode
         (:before (format nil "~a/^[[:space:]]*~{~a~^[[:space:]]+~}/{s=$~a}"
                          common (subseq tokens 0 p) (1+ p)))
         (:after  (format nil "~a/~{~a~^[[:space:]]+~}[[:space:]]*$/{s=$~a}"
                          common (subseq tokens (1+ p)) (1+ p)))
         (:around (format nil "~a/^[[:space:]]*~{~a[[:space:]]+~}[^[:space:]]*[[:space:]]+~{~a~^[[:space:]]+~}[[:space:]]*$/{s=$~a}"
                          common (subseq tokens 0 p) (subseq tokens (1+ p)) (1+ p))))))))

#+(or)
(progn
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05")
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :around)
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :after))


(defun exist (line file)
  (handler-case
      (progn (uiop:run-program `("sh" "-c" ,(format nil "grep -q ~a ~a" (squote line) (squote file))))
             1)
    (uiop:subprocess-error ()
      0)))

