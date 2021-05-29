;;; jwm-rtg.el --- Description    -*- lexical-binding: t; -*-

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

(message "loading minimal rtags")

;; (use-package company
;;   :defer 5
;;   :diminish
;;   :commands (company-mode company-indent-or-complete-common)
;;   :init
;;   (dolist (hook '(emacs-lisp-mode-hook
;;                   c-mode-common-hook))
;;     (add-hook hook
;;               #'(lambda ()
;;                   (local-set-key (kbd "<tab>")
;;                                  #'company-indent-or-complete-common))))
;;   :config
;;   ;; From https://github.com/company-mode/company-mode/issues/87
;;   ;; See also https://github.com/company-mode/company-mode/issues/123
;;   (defadvice company-pseudo-tooltip-unless-just-one-frontend
;;       (around only-show-tooltip-when-invoked activate)
;;     (when (company-explicit-action-p)
;;       ad-do-it))

;;   ;; See http://oremacs.com/2017/12/27/company-numbers/
;;   (defun ora-company-number ()
;;     "Forward to `company-complete-number'.
;;   Unless the number is potentially part of the candidate.
;;   In that case, insert the number."
;;     (interactive)
;;     (let* ((k (this-command-keys))
;;            (re (concat "^" company-prefix k)))
;;       (if (cl-find-if (lambda (s) (string-match re s))
;;                       company-candidates)
;;           (self-insert-command 1)
;;         (company-complete-number (string-to-number k)))))

;;   (let ((map company-active-map))
;;     (mapc
;;      (lambda (x)
;;        (define-key map (format "%d" x) 'ora-company-number))
;;      (number-sequence 0 9))
;;     (define-key map " " (lambda ()
;;                           (interactive)
;;                           (company-abort)
;;                           (self-insert-command 1))))

;;   (defun check-expansion ()
;;     (save-excursion
;;       (if (outline-on-heading-p t)
;;           nil
;;         (if (looking-at "\\_>") t
;;           (backward-char 1)
;;           (if (looking-at "\\.") t
;;             (backward-char 1)
;;             (if (looking-at "->") t nil))))))

;;   (define-key company-mode-map [tab]
;;     '(menu-item "maybe-company-expand" nil
;;                 :filter (lambda (&optional _)
;;                           (when (check-expansion)
;;                             #'company-complete-common))))

  ;; (eval-after-load "coq"
  ;;   '(progn
  ;;      (defun company-mode/backend-with-yas (backend)
  ;;        (if (and (listp backend) (member 'company-yasnippet backend))
  ;;            backend
  ;;          (append (if (consp backend) backend (list backend))
  ;;                  '(:with company-yasnippet))))
  ;;      (setq company-backends
  ;;            (mapcar #'company-mode/backend-with-yas company-backends))))

;;  (global-company-mode 1))


;; (use-package company-rtags
;;   ;;:disabled t
;;   ;;:load-path "~/.nix-profile/share/emacs/site-lisp/rtags"
;;     :load-path "~/.emacs.d/straight/repos/rtags/src"
;;   :after (company rtags)
;;   :config
;;   (push 'company-rtags company-backends))

;; (use-package flycheck-rtags
;;   ;;:disabled t
;;   ;;:load-path "~/.nix-profile/share/emacs/site-lisp/rtags"
;;     :load-path "~/.emacs.d/straight/repos/rtags/src"
;;   :after flycheck)

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package flyspell
;;   :bind (("C-c i b" . flyspell-buffer)
;;          ("C-c i f" . flyspell-mode))
;;   :config
;;   (defun my-flyspell-maybe-correct-transposition (beg end candidates)
;;     (unless (let (case-fold-search)
;;               (string-match "\\`[A-Z0-9]+\\'"
;;                             (buffer-substring-no-properties beg end)))
;;       (flyspell-maybe-correct-transposition beg end candidates))))

;; (use-package rtags
;;   :load-path "~/.emacs.d/straight/repos/rtags/src"
;;   :commands rtags-mode
;;   :bind (("C-c . D" . rtags-dependency-tree)
;;          ("C-c . F" . rtags-fixit)
;;          ("C-c . R" . rtags-rename-symbol)
;;          ("C-c . T" . rtags-tagslist)
;;          ("C-c . d" . rtags-create-doxygen-comment)
;;          ("C-c . c" . rtags-display-summary)
;;          ("C-c . e" . rtags-print-enum-value-at-point)
;;          ("C-c . f" . rtags-find-file)
;;          ("C-c . i" . rtags-include-file)
;;          ("C-c . i" . rtags-symbol-info)
;;          ("C-c . m" . rtags-imenu)
;;          ("C-c . n" . rtags-next-match)
;;          ("C-c . p" . rtags-previous-match)
;;          ("C-c . r" . rtags-find-references)
;;          ("C-c . s" . rtags-find-symbol)
;;          ("C-c . v" . rtags-find-virtuals-at-point))

;;   :config
;;   (use-package flycheck-rtags)
;;   :bind (:map c-mode-base-map
;;               ("M-." . rtags-find-symbol-at-point)))


;; (use-package rtags-xref)
;;       (add-hook 'c-mode-common-hook 'rtags-xref-enable)

;; see https://github.com/ZedThree/dotfiles/blob/master/emacs

(use-package company
  :config
  (global-company-mode)

  :diminish company-mode)

(use-package rtags
  :init

  ;; Start rtags automatically for C/C++
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)

  :config
  (rtags-enable-standard-keybindings)

  ;; Get completions working with company mode
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)

  (use-package company-rtags
    :init
    (push 'company-rtags company-backends))

  (use-package helm-rtags
    :init
    (setq rtags-use-helm t)

    :config
    (setq rtags-display-result-backend 'helm))
  (use-package rtags-xref)
  (add-hook 'c-mode-common-hook 'rtags-xref-enable)

  (use-package flycheck-rtags))


(provide 'jwm-rtg)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-rtg.el ends here.
