;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSD - Simple SBCL Debugger ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SB-DI  = SBCL Debug Interface

(defun top-frame ()
  (sb-di::top-frame))

(defun next-frame (frame)
  (sb-di::frame-down frame))

(defun print-frames (&optional (n 10))
  (mapcar #'print-frame
	  (iterate #'next-frame (top-frame) n))
  nil)

(defun print-frame (frame)
  (format t "Function Name: ~A~%" (function-name frame)) ;; Function Name
  (format t "Source File: ~A~%" (file-source (source (code-location frame))))
  (format t "----------------~%"))


(defun code-location (frame)
  (sb-di::frame-code-location frame))

(defun source (code-location)
  (sb-di:code-location-debug-source code-location))

(defun file-source (source)
  (sb-c::debug-source-namestring source))

(defun function-name (frame) 
  (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper common functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO find lisp equivalents?

;; clojure iterate
(defun iterate (f i n)
  "return sequence of size n: (i, f(i), f(f(i)) ...)"
  (if (and i (> n 0))
      (let ((result (funcall f i)))
	(cons i (iterate f result (1- n)))) 
      nil))

;; (iterate #'1+ 0 5) => (0 1 2 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Fuctions ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add (x y)
  (print-frames 3)
  (real-add x y))

(defun real-add (x y)
  (+ x y))


;; 1st part

;; [[DONE]] FUNCTION NAME
;; TODO ARGUMENTS
;; [[DONE]] SOURCE FILE 
;; TODO LINE NUMBER
;; TODO FILTER BAD VALUES
;; TODO DEBUGGING MACRO
