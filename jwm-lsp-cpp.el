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


;; this is great but it doesnt work:-
;; ((nil . ((eval .
;;                (setq lsp-clients-clangd-args
;;                      (concat "--compile-commands-dir=" (concat
;;                                                         (locate-dominating-file
;;                                                          default-directory
;;                                                          dir-locals-file)
;;                                                         "build_ninja/")))))))



(use-package lsp-mode
  ;; https://git.0xee.eu/0xee/emacs-config/src/commit/52435766f7bd599b02eaf5730386ea50152dc6d6/lsp.el?lang=zh-HK
  :custom (lsp-clients-clangd-args '("--compile-commands-dir=build_ninja"
                                     "--clang-tidy" "--log=verbose"))
  )

(use-package yasnippet)
(use-package lsp-treemacs)
;; (use-package helm-lsp)
(use-package projectile)
(use-package hydra)
;; (use-package flycheck)
                                        ;(use-package company)
;; (straight-use-package '(jolyon929 :type git :host bitbucket :repo "jolyon929/company-mode"))


(setq lsp-treemacs-sync-mode 1)

(use-package avy)
(use-package which-key)
;; (use-package helm-xref)
;;(use-package dap-mode)


;; I was also struggling with this recently and I think the main difficulty for me was that there are different debug programs that require different configurations. It took also way longer than it should have to figure out how to tell the debugger which file to debug and how to pass command-line arguments.

;; Here is my setup. First the relevant part from .emacs.d/init.el. Second, the file .emacs.d/default-launch.json. As you can see from the config, I use lldb-vscode, which is installed under /usr/bin/lldb-vscode.

(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t                           "Automatically configure dap.")
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  ;;:config
  ;;; dap for c++
  ;;(require 'dap-lldb)

  ;;; set the debugger executable (c++)
  ;;(setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

  ;;; ask user for executable to debug if not specified explicitly (c++)
  ;;(setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;;; default debug template for (c++)
  ;; (dap-register-debug-template
  ;;  "C++ LLDB dap"
  ;;  (list :type "lldb-vscode"
  ;;        :cwd nil
  ;;        :args nil
  ;;        :request "launch"
  ;;        :program nil))

  ;; (defun dap-debug-create-or-edit-json-template ()
  ;;   "Edit the C++ debugging configuration or create + edit if none exists yet."
  ;;   (interactive)
  ;;   (let ((filename (concat (lsp-workspace-root) "/launch.json"))
  ;;     (default "~/.emacs.d/default-launch.json"))
  ;;     (unless (file-exists-p filename)
  ;;   (copy-file default filename))
  ;;     (find-file-existing filename))))

  ;; dap-mode
  :bind (:map dap-mode-map
              ("<s-f10>" . dap-next)
              ("<s-f11>" . dap-step-in)
              ("<s-C-f11>" . dap-step-out)
              ("<s-f5>" . dap-continue)
              ("<s-f9>" . dap-breakpoint-toggle)
              ))


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

;; lsp-format-buffer
;; clang-format  --dump-config -style google

;; (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")

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

;; (use-package lsp-mode
;;   :hook
;;   ((python-mode . lsp)))

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

;; problematic!
;; (use-package flycheck-clang-tidy
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-clang-tidy-setup)
;;   )

;; problematic!

;; (use-package flycheck-clang-tidy
;;   :after flycheck
;;   :commands c++-mode
;;   :config
;;   (flycheck-clang-tidy-setup)
;;   (flycheck-add-next-checker 'lsp-ui 'c/c++-clang-tidy)
;;   )

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

;; mingw64:-

;; download nodejs to c:\nodejs
;; https://nodejs.org/en/download/

;; update .bashrc:-
;; export PATH=$PATH:/c/nodejs


(add-hook 'sh-mode-hook #'lsp)

;; pip install cmake-language-server
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(setq lsp-semantic-tokens-enable t)

(provide 'jwm-lsp-cpp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-lsp-cpp.el ends here.
;; https://www.reddit.com/r/emacs/comments/mxiqt6/how_to_setup_and_use_dapmode_for_c/

;;  (local-set-key (kbd "C-q") 'dap-quit)
;; (local-set-key (kbd "<f1>") 'dap-cont)
;; (local-set-key (kbd "<f2>") 'gud-next)
;; (local-set-key (kbd "<f3>") 'gud-step)


;; scheme:-


;; https://www.youtube.com/watch?v=OyfBQmvr2Hc
;; https://www.lvguowei.me/post/the-most-beautiful-program-ever-written/


;; https://www.youtube.com/watch?v=tA1clbGDczI
;; https://www.lvguowei.me/post/sicp-goodness-looping/
