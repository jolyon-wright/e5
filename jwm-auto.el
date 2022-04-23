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

;;(require 'jw-fctn)

(autoload 'yas-expand-snippet "yasnippet")

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (jw-make-sub-dir-name "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (setq yas-verbosity 2)

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
  (define-auto-insert "\\.prg$" ["default-org" autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh" autoinsert-yas-expand])
  (define-auto-insert "\\.el$" ["default-el" autoinsert-yas-expand])
  (define-auto-insert "\\.h$" ["default-h" autoinsert-yas-expand])
  (define-auto-insert "\\.c$" ["default-c" autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["default-py" autoinsert-yas-expand])
  (define-auto-insert "\\.cu$" ["default-cpp" autoinsert-yas-expand])
  (define-auto-insert "\\.inv$" ["default-inv" autoinsert-yas-expand])
  (define-auto-insert "\\.cpp$" ["default-cpp" autoinsert-yas-expand]))
  (define-auto-insert "\\.rkt$" ["default-rkt" autoinsert-yas-expand])
  ;; todo - above;make this better…

(use-package yasnippet

  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ;; "C-." . 'yas-expand)) ;; conflicts
              ;;       ("<f8>" . 'yas-expand))
              ("<C-tab>" . 'yas-expand))
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets)
  (add-to-list 'yas-snippet-dirs (jw-make-sub-dir-name "snippets"))
  (yas-reload-all))


(straight-use-package '(bmaland :type git :host github :repo "bmaland/yasnippet-sh-mode"))
;; https://github.com/bmaland/yasnippet-sh-mode


(straight-use-package '(brianqq :type git :host github :repo "brianqq/inferior-cling"))

(provide 'jwm-auto)
;;; jwm-auto.el ends here
