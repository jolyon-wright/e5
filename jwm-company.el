;;; jwm-company.el --- Description    -*- lexical-binding: t; -*-

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

;;

;;; Code:
;;

;; use ctrl+enter to select.
;; (straight-use-package '(jolyon929 :type git :host bitbucket :repo "jolyon929/company-mode"))

(use-package company)

(setq company-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\e\e\e" 'company-abort)
    (define-key keymap "\C-g" 'company-abort)
    (define-key keymap (kbd "M-n") 'company--select-next-and-warn)
    (define-key keymap (kbd "M-p") 'company--select-previous-and-warn)
    (define-key keymap (kbd "C-n") 'company-select-next-or-abort)
    (define-key keymap (kbd "C-p") 'company-select-previous-or-abort)
    (define-key keymap (kbd "<down>") 'company-select-next-or-abort)
    (define-key keymap (kbd "<up>") 'company-select-previous-or-abort)
    (define-key keymap [remap scroll-up-command] 'company-next-page)
    (define-key keymap [remap scroll-down-command] 'company-previous-page)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [mouse-1] 'company-complete-mouse)
    (define-key keymap [mouse-3] 'company-select-mouse)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    ;; (define-key keymap [return] 'company-complete-selection)
    ;; (define-key keymap (kbd "RET") 'company-complete-selection)
    ;; (define-key keymap [tab] 'company-complete-common)
    ;; (define-key keymap (kbd "TAB") 'company-complete-common)
    (define-key keymap (kbd "C-<return>") 'company-complete-selection)
    (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
    (define-key keymap (kbd "C-h") 'company-show-doc-buffer)
    (define-key keymap "\C-w" 'company-show-location)
    (define-key keymap "\C-s" 'company-search-candidates)
    (define-key keymap "\C-\M-s" 'company-filter-candidates)
    (company-keymap--bind-quick-access keymap)
     keymap))

(bind-key "<C-f3>" #'global-company-mode)
(setq company-global-modes '(not text-mode org-mode))
(setq company-show-quick-access t)
(global-company-mode)


(use-package company-prescient
  :init (company-prescient-mode 1))

;; https://oremacs.com/2017/12/27/company-numbers/
(provide 'jwm-company)


;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-company.el ends here.
