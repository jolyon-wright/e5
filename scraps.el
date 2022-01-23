;;; scraps.el --- Description    -*- lexical-binding: t; -*-

;; Author:   11547@AS-UK-748N693
;; Keywords:
;; URL:

;; Copyright (C) 2022, , all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defun jw-x()
  (interactive)
  (insert
   (concat "JCW:"
           (number-to-string (ts-year (ts-now)))
           "-"
           (format "%02d" (ts-month (ts-now)))
           "-"
           (format "%02d" (ts-day (ts-now)))
           " - ")))

(bind-key "s-`"  #'jw-x)

(defvar jw-counter 0)

(require 'cl-lib)

(defun  jw-ins-val ()
  (interactive)
  (insert (concat "JCW-" (format "%d" (cl-incf jw-counter)) " ")))

(bind-key "C-s-`"  #'jw-ins-val)

(setq display-raw-bytes-as-hex t)


(defun & (&rest r)
  (apply #'logand r))

(defun | (&rest r)
  (apply #'logior r))


(defun jw-int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

;; (fset #'logior jw-or )

;; (cl-flet (jw-or #'logior))

;; Introduction to Emacs Lisp - Learning Emacs Lisp #1
;; https://www.youtube.com/watch?v=RQK_DaaX34Q&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7
(type-of "")

;; Types, Conditionals, and Loops - Learning Emacs Lisp #2
;; https://www.youtube.com/watch?v=XXpgzyeYh_4&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7&index=2&t=3s

;; ielm

;; and returns last t value
;; or  returns first t value

;; pcase?


;; while
;; dotimes
;; dolist


;; Defining Functions and Commands - Learning Emacs Lisp #3
;; https://www.youtube.com/watch?v=EqgkAUHw0Yc&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7&index=3

;; Defining Variables and Scopes - Learning Emacs Lisp #4
;; https://www.youtube.com/watch?v=tq4kTNL1VD8&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7&index=4

;; (use-package emacs) ;; :custom


;; https://www.youtube.com/watch?v=J7d2LmivyyM&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7&index=5




;; Reading and Writing Buffers in Practice - Learning Emacs Lisp #5
  ;; buffer local variables

;; #6 files and directories



(defun jw-tmp()
  (with-current-buffer "*scratch*"
    (message "cur buf:%s" (current-buffer))
    )
  (message "cur buf:%s" (current-buffer))
  )

;; find-file-noselect -- open file in invisible buffer

;; or -- used like if else else

;; the point


;; ELISP> (buffer-substring 1 21)
;; #("*** Welcome to IELM " 0 20
;;   (face font-lock-comment-face fontified t inhibit-line-move-field-capture t field output rear-nonsticky t))

;; ELISP> (buffer-substring-no-properties 1 21)
;; "*** Welcome to IELM "
;; ELISP>

;; (search-backward "holy smoke")

;; https://www.youtube.com/watch?v=nq-gqNGmayI&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7&index=6

;; (defun jw-tmp (&rest args)
;;   (print "confusion")
;;   (print args))
  ;; (apply #'print args))
  ;; (apply #'print args))
  ;; (dolist(itm args)
  ;;   (print itm)))

;; (jw-tmp '("egg" "chips"))

(provide 'scraps)

;; Local Variables:
;; coding: utf-8
;; End:

;;; scraps.el ends here.


;; https://osxdaily.com/2017/11/16/access-icloud-drive-command-line-mac/
