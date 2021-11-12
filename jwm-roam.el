;;; jwm-roam.el --- Description    -*- lexical-binding: t; -*-

;; Author: Jolyon Wright  jolyon@Jolyons-MacBook-Pro.local
;; Keywords:
;; URL:

;; Copyright (C) 2021, Jolyon Wright, all rights reserved.

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

;; https://lucidmanager.org/productivity/taking-notes-with-emacs-org-mode-and-org-roam/

;; also - helm https://lucidmanager.org/productivity/emacs-completion-system/


;;; Code:

  ;; Org-Roam basic configuration
  ;; (setq org-directory (concat (getenv "HOME") "/Documents/org-roam/"))

(unless (file-exists-p "~/org-roam")
  ;; todo - use git or something
  (make-directory "~/org-roam"))

(setq org-roam-directory (file-truename "~/org-roam"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  ;; (org-roam-setup)

  (org-roam-db-autosync-mode)

  ;; todo - use a prefix; eg
  ;; :prefix "<f3>"
  ;;  :prefix-map jw-prefix-map
  ;;  ("h" . org-html-export-to-html)
  ;;  ("p" . org-latex-export-to-pdf)
  ;;  ("m" . org-gfm-export-to-markdown)
  ;;  ("c" . org-confluence-export-as-confluence))


  ;; :bind (("C-c n f" . org-roam-node-find)
  ;;        ("C-c n g" . org-roam-graph)
  ;;        ("C-c n r" . org-roam-node-random)
  ;;        (:map org-mode-map
  ;;              (("C-c n i" . org-roam-node-insert)
  ;;               ("C-c n o" . org-id-get-create)
  ;;               ("C-c n t" . org-roam-tag-add)
  ;;               ("C-c n a" . org-roam-alias-add)
  ;;               ("C-c n l" . org-roam-buffer-toggle))))
  )


(provide 'jwm-roam)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-roam.el ends here.
