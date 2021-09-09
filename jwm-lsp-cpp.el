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

;; mingw64 on windows

;; pacman -S unzip
;;;; loading cpp file causes lsp error:-
;; Contacting host: github.com:443
;; Wrote c:/jolyon/.emacs.d/.cache/lsp/clangd/clangd.zip.zip
;; LSP :: Finished downloading c:/jolyon/.emacs.d/.cache/lsp/clangd/clangd.zip.zip...
;; LSP :: Decompressing c:/jolyon/.emacs.d/.cache/lsp/clangd/clangd.zip.zip...
;; LSP :: Server clangd install process failed with the following error message: (error Unable to find ‘unzip’ or ‘powershell’ on the path, please customize ‘lsp-unzip-script’).
;; Check `*lsp-install*' and `*lsp-log*' buffer.
;; cmake-ide [Mon Sep  6 20:53:34 2021]: Finished running CMake


;; cd c:/jolyon/.emacs.d/.cache/lsp/clangd
;; unzip clangd.zip.zip

;; now all fine; reload the CPP file
;;
;; pacman -S mingw-w64-x86_64-ninja

;; invoke:
;; cmake -G Ninja .
;; ninja
;;
;; all is well


(use-package lsp-mode)
(use-package yasnippet)
;; (use-package lsp-treemacs)
;; (use-package helm-lsp)
(use-package projectile)
(use-package hydra)
(use-package flycheck)
                                        ;(use-package company)
;; (straight-use-package '(jolyon929 :type git :host bitbucket :repo "jolyon929/company-mode"))

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
  (require 'dap-cpptools)
  (setq lsp-prefer-flymake nil)
  (yas-global-mode))

;; https://www.mattduck.com/lsp-python-getting-started.html
;; pip3 install python-language-server[all]

(use-package lsp-mode
  :hook
  ((python-mode . lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode)



;; Some integrations are not available by default in pyls, but are supported by plugins. You can install these with pip install pyls-black pyls-isort pyls-mypy.

;; To then enable them in lsp-mode, you can use (lsp-register-custom-settings):

;; (use-package lsp-mode
;;   :config
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)))
;;   :hook
;;   ((python-mode . lsp)))

;; Optional - provides fancier overlays.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)


;; worth trying:-
;; lsp-ui
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap save-buffer] . my/lsp-format-and-save)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-position 'bottom)
  (defun my/lsp-format-and-save ()
    (interactive)
    (lsp-format-buffer)
    (save-buffer)))



;; (require 'rsz-mini)
;; (resize-minibuffer-mode 1)

(setq resize-mini-windows t)

; Wrap lines in minibuffer mode
;; (add-hook 'minibuffer-setup-hook
;;           '(lambda ()
;;              (setq truncate-lines nil)))

;; (use-package flycheck
;; :ensure t
;; :init (global-flycheck-mode))
;; flycheck
;; http://www.flycheck.org
;; Dep flake8, clang, tidy, csslint
(use-package flycheck
  :defer 2
  :bind
  (:map flycheck-mode-map ("C-c ! !" . hydra-flycheck/body))
  ("M-g l" . flycheck-list-errors)
  :config
  (global-flycheck-mode)
  (setq-default flycheck-global-modes '(not org-mode))
  ;; hydra
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter  "Filter")
    ("n"  flycheck-next-error             "Next")
    ("p"  flycheck-previous-error         "Previous")
    ("q"  nil                             "Quit")))


;; flycheck-inline
;; https://github.com/flycheck/flycheck-inline
(use-package flycheck-inline
  :after flycheck
  :hook
  (flycheck-mode . flycheck-inline-mode))

;; (use-package company-shell
;;     :defer 3
;;     :config
;;     (add-to-list 'company-backends 'company-shell)
;;     )


;; brew install node

;; ubuntu 2004:-

;; sudo apt install npm
;; sudo npm cache clean -f
;; sudo npm install -g n
;; sudo n stable


(add-hook 'sh-mode-hook #'lsp)




(provide 'jwm-lsp-cpp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-lsp-cpp.el ends here.
