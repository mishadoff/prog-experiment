;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSD - Simple SBCL Debugger ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SB-DI  = SBCL Debug Interface

(defun top-frame ()
  (sb-di::top-frame))

(defun next-frame (frame)
  (sb-di::frame-down frame))

(defun print-frames (&optional (n 2))
  (mapcar #'print-frame
	  (rest (iterate #'next-frame (top-frame) n)))
  nil)

(defun print-frame (frame)
  (let* ((fun-name (function-name frame))
	 (args-list (frame-args frame))
	 (file-name (source-file-name frame))
	 (line-number (source-function-matches 
		       (concatenate 'string "(defun " (string fun-name))
	  	       (source-fun-lines file-name))))
    (format t "Function Name: ~A~%" fun-name)
    (format t "ARGS[~D]: ~{~A ~}~%" (length args-list) args-list)
    (format t "Source File: ~A:~D~%" file-name line-number)
    (format t "----------------~%")))

;; Frame extractors

(defun function-name (frame)
  (let ((fname (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))))
    (if (atom fname) fname
	(second fname))))

(defun frame-args (frame)
  (sb-debug::frame-args-as-list frame))

(defun source-file-name (frame)
  (sb-c::debug-source-namestring
   (frame-source-location frame)))

(defun frame-source-location (frame)
  (sb-di:code-location-debug-source 
    (sb-di::frame-code-location frame)))

(defun source-fun-lines (file)
  (with-open-file (in file :if-does-not-exist nil)
    (if in
	(loop for line = (read-line in nil 'eof)
	   until (eq line 'eof)
	   collect line) nil)))

(defun source-function-matches (fun-name lines &optional (c 1))
  (if lines
      (if (search (string-upcase fun-name) (string-upcase (first lines))) c
	  (source-function-matches fun-name (rest lines) (1+ c))) 0))

;; Helper common functions

;; TODO find lisp equivalents?

;; clojure iterate
(defun iterate (f i n)
  "return sequence of size n: (i, f(i), f(f(i)) ...)"
  (if (and i (> n 0))
      (let ((result (funcall f i)))
	(cons i (iterate f result (1- n)))) 
      nil))

;; (iterate #'1+ 0 5) => (0 1 2 3 4)

;; Test Functions

(defun add (x y)
  (print-frames)
  (real-add x y))

(defun real-add (x y)
  (print-frames)
  (+ (I x) (I y)))

(defun I (x) 
  (print-frames)
  x)

(defun random-sum ()
  (print-frames)
  (add (random 5) (random 5)))

(defun random-sum-with-failure ()
  (print-frames)
  (let ((r (add (random 5) (random 5))))
    (if (< r 6) r (error "RESULT IS GREATER THAN 6"))))

;; 1st part

;; [[DONE]] FUNCTION NAME
;; [[DONE]] ARGUMENTS
;; [[DONE]] SOURCE FILE 
;; [[DONE]] LINE NUMBER
;; TODO DEBUGGING MACRO


;; EXAMPLE

;; CL-USER> (random-sum)
;; Function Name: RANDOM-SUM
;; ARGS[0]: 
;; Source File: /home/mishadoff/coding/prog-experiment/lisp/debugger.lisp:95
;; ----------------
;; Function Name: ADD
;; ARGS[2]: 1 2 
;; Source File: /home/mishadoff/coding/prog-experiment/lisp/debugger.lisp:83
;; ----------------
;; Function Name: REAL-ADD
;; ARGS[2]: 1 2 
;; Source File: /home/mishadoff/coding/prog-experiment/lisp/debugger.lisp:87
;; ----------------
;; Function Name: I
;; ARGS[1]: 1 
;; Source File: /home/mishadoff/coding/prog-experiment/lisp/debugger.lisp:69
;; ----------------
;; Function Name: I
;; ARGS[1]: 2 
;; Source File: /home/mishadoff/coding/prog-experiment/lisp/debugger.lisp:69
;; ----------------
;; 3
