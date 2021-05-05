;;; jwm-auto.el --- attempt to use auto insert       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jolyon Wright

;; Author: Jolyon Wright <jolyon@Jolyons-MacBook-Pro.local>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'jw-fctn)

(autoload 'yas-expand-snippet "yasnippet")

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (make-sub-dir-name "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config

  ;; ../templates/default-lisp.el should look something like this:-

  ;;; `(buffer-name)` --- ${1:Description}
  ;;
  ;; Author: `user-full-name`  `user-mail-address`
  ;;
  ;; blah blah
  ;;
  ;; (provide '`(substring (buffer-name) 0  (- (length (buffer-name)) 3))`)
  ;;
  (define-auto-insert "\\.lisp$" ["default-lisp" autoinsert-yas-expand])
  (define-auto-insert "\\.org$" ["default-org" autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh" autoinsert-yas-expand])
  (define-auto-insert "\\.el$" ["default-el" autoinsert-yas-expand]))


(use-package yasnippet

  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-." . 'yas-expand))
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets)
  (add-to-list 'yas-snippet-dirs (make-sub-dir-name "snippets"))
  (yas-reload-all))

(provide 'jwm-auto)
;;; jwm-auto.el ends here
