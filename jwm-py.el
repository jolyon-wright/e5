;;; jwm-py.el --- Description    -*- lexical-binding: t; -*-

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
;; python https://stackoverflow.com/questions/42486695/python-type-hinting-in-emacs
;; Install pyflakes and mypy using pip.
;;
;; mac/ububtu:-
;; pip3 install pyflakes
;; mac:-
;; brew install mypi
;; ubuntu:-
;; pip3 install

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (global-flycheck-mode t)
;;   ;; note that these bindings are optional
;;   (global-set-key (kbd "C-c n") 'flycheck-next-error)
;;   ;; this might override a default binding for running a python process,
;;   ;; see comments below this answer
;;   (global-set-key (kbd "C-c p") 'flycheck-prev-error)
;;   )
;; ;; flycheck-pycheckers
;; ;; Allows multiple syntax checkers to run in parallel on Python code
;; ;; Ideal use-case: pyflakes for syntax combined with mypy for typing
;; (use-package flycheck-pycheckers
;;   :after flycheck
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
;;     )
;;   (setq flycheck-pycheckers-checkers
;;     '(
;;       mypy3
;;       pyflakes
;;       )
;;     )
;;   )
;; ;; elpy
;; (use-package elpy
;;   :after poetry
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (add-hook 'elpy-mode-hook 'poetry-tracking-mode) ;; optional if you're using Poetry
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (setq elpy-syntax-check-command "~/.pyenv/shims/pyflakes") ;; or replace with the path to your pyflakes binary
;;   ;; allows Elpy to see virtualenv
;;   (add-hook 'elpy-mode-hook
;;         ;; pyvenv-mode
;;         '(lambda ()
;;            (pyvenv-mode +1)
;;            )
;;         )
;;   ;; use flycheck instead of flymake
;;   (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )
;; ;; poetry
;; (use-package poetry
;;   :ensure t)

;; (use-package company-jedi)

;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; ATTEMPT TO GET LANGUAGE SERVER PROTOCOL CLIENT SET UP

;; needs a system clangd, which itself requires clang and libtool

(straight-use-package 'cmake-mode)

;; Hide warning "LSP :: Yasnippet is not installed, but `lsp-enable-snippet' is set to `t'."
(setq lsp-enable-snippet nil)

;; disable the annoying top bar
(setq lsp-headerline-breadcrumb-enable nil)

;; flymake causes errors when loading a saved desktop "error in process sentinel: flymake-error ..."
(straight-use-package 'flycheck)
;; autocompletion frontend
(straight-use-package 'company)
(straight-use-package 'company-lsp)
(straight-use-package 'lsp-mode)

; we really want to bail out based on the time clangd takes to process the file
(defun jw-lsp-if-small-file ()
  (let*
    (
    (limit 6000)
    (limit-str (number-to-string limit))
    (num-lines (count-lines (point-min) (point-max)))
    (large-file-msg (concat "JW-LSP: File " limit-str "+ lines, no clangd"))
    )
    (if (<= num-lines limit)
      ;; this is significantly faster if many C files open
      (lsp-deferred)
      (message large-file-msg)
    )
  )
)

(add-hook 'c-mode-hook 'jw-lsp-if-small-file)

;; lsp python mumbo jumbo
(straight-use-package 'lsp-pyright)

;; hackery to avoid other bugs
(setq lsp-enable-file-watchers nil)
;; (defun jw-py-lsp () (interactive) (require 'lsp-pyright) (lsp))
(add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp)))

;; python virtual environment (pip3 install virtualenv)
(straight-use-package 'pyvenv)

;; dap mode
;; pip install "ptvsd>=4.2"

(straight-use-package 'dap-mode)
(require 'dap-python)

;; (dap-auto-configure-mode)

;; vscode-like keybindingbs
;; (global-set-key (kbd "C-d") 'dap-debug)
;; (global-set-key (kbd "C-b") 'dap-breakpoint-toggle)
;; (global-set-key (kbd "<f5>") 'dap-continue)
;; (global-set-key (kbd "<f10>") 'dap-next)
;; (global-set-key (kbd "<f11>") 'dap-step-in)
;; (global-set-key (kbd "<escape>") 'dap-disconnect)


;; (bind-key "" 'dap-debug)

(bind-keys :map dap-mode-map
           :prefix "<f3>"
           :prefix-map jw-dap-mode-prefix-map
           ;;("d" . dap-debug)
           ("b" . dap-breakpoint-toggle)
           ("e" . dap-continue)
           ("n" . dap-next)
           ("i" . dap-step-in)
           ("d" . dap-disconnect))



;; dap-disconnect exits the debugging session
;; (defun jw-py-debug () (interactive) (dap-debug "JWPython"))
(defun jw-py-debug () (interactive) (dap-debug (list :type "python"
                                                     :args ""
                                                     :cwd nil
                                                     :module nil
                                                     :program nil
                                                     :request "launch"
                                                     :name "JWPython")))

;; https://docs.hylang.org/en/stable/
;; pip3 install --user hy
;; brew install hy
;;
;; https://kitchingroup.cheme.cmu.edu/blog/2016/03/31/More-on-Hy-and-why-I-think-it-is-a-big-deal/
(use-package hy-mode)

;; pip install jedhy



;; https://kitchingroup.cheme.cmu.edu/blog/2016/04/01/ob-hy-el-or-better-integration-of-hylang-in-org-mode/


;; https://leanpub.com/hy-lisp-python/read
;; https://www.pythonpodcast.com/episode-23-hylang-core-developers/

;; https://leanpub.com/lovinglisp/read
;; https://github.com/mark-watson/loving-common-lisp.git

;; example.hy:-
;; #!/usr/bin/env hy

;; (require [hy.contrib.walk [let]])


;; https://www.fast.ai/

(provide 'jwm-py)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-py.el ends here.
