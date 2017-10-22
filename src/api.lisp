
(in-package :dirtylogman)

;;; API brainstorming
;; as a command line tool, I decided to make the main interface as a yaml file. Below was other alternatives:
;; Given a file... e.g.
;; ./gaussian/latplan.puzzles.puzzle_mnist/007-003-000/puzzle_mnist_3_3_36_20000_conv_Astar.log


;; access methods

#|
;; like a pattern?
(and path
     (concatenate _ "/" noise "/" _ "." _ "." track "/" steps "-" no "-" _ "/" _)
     (extract (read time) "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05")
     (extract (read expansion) "Expanded 5 state(s)." "5" :mode :around)
     (exist   solution "Solution found!")
     (count   (read solution) "Solution found!")
     (shell "wc -l"))

;; more like a dsl?
(:as path
     (:split _ "/" noise "/" _ "." _ "." track "/" steps "-" no "-" _ "/" _)
     (:extract ...))

;; more like yaml?
(and path
     (concatenate _ "/" noise "/" _ "." _ "." track "/" steps "-" no "-" _ "/" _)
     (extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :as time)
     (extract "Expanded 5 state(s)." "5" :mode :around :as expansion)
     (exist "Solution found!" :as solution)
     (count "Solution found!" :as solution-count)
     (shell "wc -l"))

;; path as a default?
(concatenate _ "/" noise "/" _ "." _ "." track "/" steps "-" no "-" _ "/" _)
(extract "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05" :as time)
(extract "Expanded 5 state(s)." "5" :mode :around :as expansion)
(exist "Solution found!" :as solution)
(count "Solution found!" :as solution-count)
(shell "wc -l")

(concatenate _ "/" noise "/" _ "." _ "." track "/" steps "-" no "-" _ "/" _)
(awk (like "Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]" "1.991e-05") :as time)
(awk (like "Expanded 5 state(s)." "5" :mode :around) :as expansion)
(exist "Solution found!" :as solution)
(count "Solution found!" :as solution-count)
(shell "wc -l")

|#

;;; sample yaml parse input


(defun read-yaml (pathname)
  (yaml:parse (read-file-into-string pathname)))

;; hashtable by default

#+(or)
(("numline" "shell \"wc -l\"") ("solution-count" "count \"Solution found!\"")
 ("solution" "exists \"Solution found!\"") ("expansion" "like \"Expanded 5 state(s).\" \"5\" :mode :around")
 ("time" "like \"Actual search time: 1.991e-05 (sec) [t=0.0441942 (sec)]\" \"1.991e-05\"")
 ("path" "concatenate _ \"/\" noise \"/\" _ \".\" _ \".\" track \"/\" steps \"-\" no \"-\" _ \"/\" _"))

(defun process-path (db)
  (let ((rule (gethash "path" db)))
    
