;;; jeez.el --- Description    -*- lexical-binding: t; -*-

;; Author: Jolyon Wright  jolyon@Jolyons-MacBook-Pro.local
;; Keywords:
;; URL:

;; Copyright (C) 2022, Jolyon Wright, all rights reserved.

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

;; https://rajpatil.dev/tools/CFam-lsp-mode/
(defun clangd-lsp-setup ()
  (interactive)
  ;;check if database already exists
  (let* ((dir default-directory)
	 ;;system specific params
	 (include-path-1 "/usr/include/c++/")
	 (include-path-2 "/usr/include/x86_64-linux-gnu/c++/")
	 (ver (caddr (directory-files include-path-1)))
	 (includes-str (concat "-I" (concat include-path-1 ver) "/\n"
			       "-I" (concat include-path-2 ver) "/\n"))
	 (compilation-db (concat dir "compile_flags.txt")))
    (if (file-exists-p compilation-db)
	(message "compilation database already exists")
      (progn (message "placing a new compilation database")
	     (write-region includes-str nil compilation-db)))))

(general-add-hook 'c++-mode-hook
                  (list#'clangd-lsp-setup))

;; jw - can we put this in a dir locals or somesuch

(provide 'jeez)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jeez.el ends here.
