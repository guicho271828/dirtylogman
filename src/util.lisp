
(in-package :dirtylogman)

(defun regex-escape (s)
  (coerce (iter (for c in-string s)
                (if (find c "()[]{}?.+*\\^$/|")
                    (appending (list #\\ c))
                    (collect c)))
          'string))


(defun nsubseq (string start &optional (end (length string)))
  (make-array (- end start)
              :displaced-to string
              :displaced-index-offset start
              :element-type (array-element-type string)))

(defun extract (line target &key (mode :before))
  "Returns an awk script as a string which extracts TARGET from LINE.
MODE specifies which context it should consider.

 (extract \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\") -> 

BEGIN{s=-1}END{if(s!=0){exit 1}};/^Actual search time: /{print $4 ; s=0; exit 0}

"
  (let* ((tokens (shellwords:split line))
         (token-pos (position target tokens :test 'equal))
         (char-pos (search target line))
         (common "BEGIN{s=-1}END{if(s!=0){exit 1}}"))
    (assert token-pos)
    (ecase mode
      (:before (format nil "~a;/^~a/{print $~a ; s=0; exit 0}"
                       common
                       (regex-escape (nsubseq line 0 char-pos))
                       (1+ token-pos)))
      (:after (format nil "~a;/~a$/{print $~a ; s=0; exit 0}"
                      common
                      (regex-escape (nsubseq line (+ (length target) char-pos)))
                      (1+ token-pos)))
      (:around (format nil "~a;/^~a.*~a$/{print $~a ; s=0; exit 0}"
                       common
                       (regex-escape (nsubseq line 0 char-pos))
                       (regex-escape (nsubseq line (+ (length target) char-pos)))
                       (1+ token-pos))))))

#+(or)
(progn
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05")
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :around)
  (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :mode :after))



(defun extract-all (line target &key (mode :before))
  "A variant of EXTRACT which extracts all appearances of the pattern from the input."
  (let* ((tokens (shellwords:split line))
         (token-pos (position target tokens :test 'equal))
         (char-pos (search target line))
         (common "BEGIN{s=-1}END{if(s!=0){exit 1}}"))
    (assert token-pos)
    (ecase mode
      (:before (format nil "~a;/^~a/{print $~a ; s=0}"
                       common
                       (regex-escape (nsubseq line 0 char-pos))
                       (1+ token-pos)))
      (:after (format nil "~a;/~a$/{print $~a ; s=0}"
                      common
                      (regex-escape (nsubseq line (+ (length target) char-pos)))
                      (1+ token-pos)))
      (:around (format nil "~a;/^~a.*~a$/{print $~a ; s=0}"
                       common
                       (regex-escape (nsubseq line 0 char-pos))
                       (regex-escape (nsubseq line (+ (length target) char-pos)))
                       (1+ token-pos))))))
