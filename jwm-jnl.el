;;; jwm-jnl.el --- Description    -*- lexical-binding: t; -*-

;; Author: jw  jw@jw-XPS-13-9370
;; Keywords:
;; URL:

;; Copyright (C) 2021, jw, all rights reserved.

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
;; https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/

;;   (setq org-modules '(...
;;                       org-crypt
;;                       ...))
;; '(org-load-modules-maybe t)

;;   (use-package org
;;     :bind ("C-c d" . org-decrypt-entry)
;;     :init (org-crypt-use-before-save-magic)
;;     :custom
;;     (org-tags-exclude-from-inheritance (quote ("crypt")))
;;     (org-crypt-key "E9AADC36E94A672D1A07D49B208FCDBB98190562")
;;     (auto-save-default nil))

(use-package org-journal
  :after org
  :bind (("C-c T" . org-journal-new-entry)
         ("C-c Y" . journal-file-yesterday))
  :preface
  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))
  :custom
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-dir (format "~/.personal/journal/" (format-time-string "%Y")))
  (org-journal-enable-encryption t)
  (org-journal-file-format "%Y%m%d")
  (org-journal-time-format ""))

(provide 'jwm-jnl)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-jnl.el ends here.
