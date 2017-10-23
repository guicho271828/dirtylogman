
(in-package :dirtylogman)

(defun nsubseq (string start &optional (end (length string)))
  (make-array (- end start)
              :displaced-to string
              :displaced-index-offset start
              :element-type (array-element-type string)))

(defun extract (line target &key (mode :before))
  "Returns an awk script as a string which extracts TARGET from LINE.
MODE specifies which context it should consider.

 (extract \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\") -> 

\"BEGINFILE{s=-1}ENDFILE{print s}/^[[:space:]]*Actual[[:space:]]+search[[:space:]]+time:/{s=$4}\"

"
  (let* ((tokens (shellwords:split line))
         (token-pos (position target tokens :test 'equal))
         (char-pos (search target line))
         (common "BEGINFILE{s=-1}ENDFILE{print s}"))
    (assert token-pos)
    (ecase mode
      (:before (format nil "~a/^~a/{s=$~a}"
                       common
                       (shellwords:escape (nsubseq line 0 char-pos))
                       (1+ token-pos)))
      (:after (format nil "~a/~a$/{s=$~a}"
                      common
                      (shellwords:escape (nsubseq line (+ (length target) char-pos)))
                      (1+ token-pos)))
      (:around (format nil "~a/^~a.*~a$/{s=$~a}"
                       common
                       (shellwords:escape (nsubseq line 0 char-pos))
                       (shellwords:escape (nsubseq line (+ (length target) char-pos)))
                       (1+ token-pos))))))

#+(or)
(progn
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05")
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :around)
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :after))



