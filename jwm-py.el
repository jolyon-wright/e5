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
;; pip3 install pyflakes
;; brew install mypi

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  ;; note that these bindings are optional
  (global-set-key (kbd "C-c n") 'flycheck-next-error)
  ;; this might override a default binding for running a python process,
  ;; see comments below this answer
  (global-set-key (kbd "C-c p") 'flycheck-prev-error)
  )
;; flycheck-pycheckers
;; Allows multiple syntax checkers to run in parallel on Python code
;; Ideal use-case: pyflakes for syntax combined with mypy for typing
(use-package flycheck-pycheckers
  :after flycheck
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )
  (setq flycheck-pycheckers-checkers
    '(
      mypy3
      pyflakes
      )
    )
  )
;; elpy
(use-package elpy
  :after poetry
  :ensure t
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'poetry-tracking-mode) ;; optional if you're using Poetry
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-syntax-check-command "~/.pyenv/shims/pyflakes") ;; or replace with the path to your pyflakes binary
  ;; allows Elpy to see virtualenv
  (add-hook 'elpy-mode-hook
        ;; pyvenv-mode
        '(lambda ()
           (pyvenv-mode +1)
           )
        )
  ;; use flycheck instead of flymake
  (when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  )
;; poetry
(use-package poetry
  :ensure t)

(use-package company-jedi)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(provide 'jwm-py)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-py.el ends here.
