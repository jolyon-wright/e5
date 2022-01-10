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


(provide 'scraps)

;; Local Variables:
;; coding: utf-8
;; End:

;;; scraps.el ends here.
