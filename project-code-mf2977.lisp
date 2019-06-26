(quicklisp:quickload :imago)
(quicklisp:quickload :drakma)
(quicklisp:quickload :opticl)
(defpackage :cover-topper (:use :cl :drakma :imago :opticl))
(in-package :cover-topper)
(defvar wth 250)
(defvar hgt 250)
(defvar cols 2)
(defvar rows 2)

(deftype binary () '(unsigned-byte 8))
(defun split-by-one-newline (string)
  "Returns a list of substrings of string
divided by ONE newline each.
Note: Two consecutive spaces will be seen ash
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\newline string :start i)
        collect (subseq string i j)
        while j))

(defun read-file-to-string (pathname)
(with-open-file (in pathname :direction :input
                             :external-format :utf-8)
  (let ((seq (make-string (file-length in))))
    (read-sequence seq in)
    seq)))

(defvar raw_string (read-file-to-string "albums.txt"))
(defvar names  (split-by-one-newline raw_string))

(defun create-chart ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height (* cols wth)) (width (* rows hgt)))
    (let ((img (make-8-bit-rgb-image height width)))
      (declare (type 8-bit-rgb-image img))
      (fill-image img 0 0 0)
      img)))

(defun save-chart ()
  (let ((img (create-chart)))
    (write-png-file "chart.png" img)))

(defvar canvas (save-chart))

(defun download-and-save-covers (lst)
  (loop for i from 0 to (- (length lst) 2)
        do
          (with-open-file (my-stream (format nil "~D.jpg" i)
                      :direction :output
                      :element-type 'binary
                      :if-does-not-exist :create
                      :if-exists :supersede)
           (let ((content (drakma:http-request (format nil "https://coverartarchive.org/release-group/~A/front-250.jpg" (nth i lst)))))
             (loop for j across content do
               (write-byte j my-stream))))
          (write-png-file (format nil "~D.png" i) (read-jpeg-file (format nil "~D.jpg" i)))))
;; that thing in front of the front-250 is the MBID, distinguishes each album or "release group"

(download-and-save-covers names)

(defun apply-covers (base lst)
  (let ((base-of-chart (read-png base)))
  (loop for i from 0 to (- (length lst) 2)
        do  (let ((operator (imago::default-compose-operator base-of-chart))
                  (cover (read-png (format nil "~D.png" i))))
                  (write-png (compose nil base-of-chart cover (+ 0 (* i wth)) 0 operator) base)))))

(apply-covers "chart.png" names)
