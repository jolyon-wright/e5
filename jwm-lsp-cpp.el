;;; jwm-lsp-cpp.el --- Description    -*- lexical-binding: t; -*-

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

;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/

;;; Code:
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)

;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
;;     projectile hydra flycheck company avy which-key helm-xref dap-mode))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))


(use-package lsp-mode)
(use-package yasnippet)
;; (use-package lsp-treemacs)
;; (use-package helm-lsp)
(use-package projectile)
(use-package hydra)
(use-package flycheck)
(use-package company)
(use-package avy)
(use-package which-key)
;; (use-package helm-xref)
(use-package dap-mode)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-headerline-breadcrumb-enable nil)

;; (setq lsp-headerline-breadcrumb-mode nil)

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (straight-use-package 'helm-xref)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)               ;
  (yas-global-mode))

;; Optional - provides fancier overlays.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (require 'rsz-mini)
;; (resize-minibuffer-mode 1)

(setq resize-mini-windows t)

; Wrap lines in minibuffer mode
;; (add-hook 'minibuffer-setup-hook
;;           '(lambda ()
;;              (setq truncate-lines nil)))

(use-package flycheck
:ensure t
:init (global-flycheck-mode))

(provide 'jwm-lsp-cpp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-lsp-cpp.el ends here.
