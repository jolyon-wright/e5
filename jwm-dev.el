;;; -*- lexical-binding: t; -*-

(use-package cmake-mode :defer)
(use-package dtrt-indent :defer)
(use-package modern-cpp-font-lock)

;; todo:-
;; https://github.com/ericniebler/range-v3
;; vcpkg
;; https://github.com/fmtlib/fmt

;; (require 'clang-format)
;; (setq clang-format-style-option "google")

;; https://nilsdeppe.com/posts/emacs-c++-ide2

(use-package clang-format+
  ;; this will look for a file called .clang-format
  ;; and apply those rules whenever the file is saved

  ;; I have tried it without cc mode styles and it is fine;
  ;; how those two play together is one for the long winter evenings!
  :hook (c-mode-common . clang-format+-mode))

;; (global-set-key (kbd "C-c C-f") 'clang-format-region)

(require 'jw-google-c-style)

;; (sp-local-pair 'c-mode "{" nil :post-handlers '(:add ("||\n[i]" "RTE")))

;;(require 'cmake-mode)
;; (use-package cmake-mode
;;   :defer)

;; (jw-safe-wrap
;;  (straight-use-package 'flymake-google-cpplint)
;;  (flymake-google-cpplint-load)
;;  (add-hook 'c++-mode-hook 'flymake-google-cpplint-load)
;;  (custom-set-variables
;;   '(flymake-google-cpplint-verbose "3")
;;   '(flymake-google-cpplint-linelength "120")
;;   )
;;  )

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(add-hook 'c-mode-common-hook 'jw-google-set-c-style)
(add-hook 'c-mode-common-hook 'jw-google-make-newline-indent)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))

(add-hook 'c++-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            ))
;; (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

; these tweaks shouldnt cause any conflicts with the google mode
(add-hook 'c-mode-common-hook
	  (lambda()
		(c-toggle-auto-newline 1)
		(c-toggle-hungry-state 1)
		(c-toggle-electric-state 1)
		(setq indent-tabs-mode nil)
        )
      )

(add-hook 'c-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

;; (jw-safe-wrap
;;  (electric-operator-add-rules-for-mode 'c-mode
;;                                        (cons "*" nil))

;; (add-hook 'c-mode-common-hook #'electric-operator-mode))

;; https://www.doof.me.uk/2019/06/09/making-emacs-gud-usable/
(setq gdb-many-windows t
      gdb-use-separate-io-buffer t)

(setq gdb-command-name "gdb")
(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

(defconst gud-window-register 123456)

(defun jw-gdb-other-frame()
  (interactive)
  (select-frame (make-frame))
  (call-interactively 'gdb))

(defun gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

;; (bind-keys ("<f9>"    . gud-cont)
;;              ("<f10>"   . gud-next)
;;              ("<f11>"   . gud-step)
;; https://github.com/ptrv/emacs.d/blob/master/init.el
(add-hook 'gud-mode-hook
          (lambda ()
            (gud-tooltip-mode)
            (window-configuration-to-register gud-window-register)
            (local-set-key (kbd "C-q") 'gud-quit)
            (local-set-key (kbd "<f1>") 'gud-cont)
            (local-set-key (kbd "<f2>") 'gud-next)
            (local-set-key (kbd "<f3>") 'gud-step)
            ))

(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (jump-to-register gud-window-register)
                (bury-buffer))))


;; (use-package projectile)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (projectile-mode +1)

(use-package flycheck)
(use-package cmake-ide)
(use-package dash)
(use-package helm)
(use-package dts-mode)
(use-package help-fns+)

(cmake-ide-setup)

(bind-key "<f7>" 'compile)
(bind-key "<C-f8>" 'align)
(bind-key "<f5>" 'gdb)
(bind-key "<C-f5>" 'jw-gdb-other-frame)

;; (use-package groovy-emacs-mode)

(straight-use-package '(lewang :type git :host github :repo "lewang/command-log-mode"))
;; clm/open-command-log-buffer to view buffer

(use-package highlight-indentation
  :hook
  (yaml-mode . highlight-indentation-mode)

  :config
  (set-face-attribute 'highlight-indentation-face nil
            :background "gray27"
            :foreground "white"))

;; (use-package yaml-mode)
(use-package indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)
(use-package highlight-symbol)

(use-package yaml-mode
  ;; http://www.wilfred.me.uk/.emacs.d/init.html
  :config
  (define-key yaml-mode-map (kbd "M-n") #'highlight-symbol-next)
  (define-key yaml-mode-map (kbd "M-p") #'highlight-symbol-prev)
  (define-key yaml-mode-map (kbd "M-N") #'highlight-symbol-last)
  (define-key yaml-mode-map (kbd "M-P") #'highlight-symbol-first))

;; (set-face-background 'highlight-indentation-face "#e3e3d3")
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(defun indent-tools-indentation-of-python ()
  "Return Python's current indentation as an int, usually 4."
  (cond ((and (boundp 'python-indent-offset)
              (numberp python-indent-offset))
         python-indent-offset)))

;; The alist.
(setq indent-tools-indentation-of-modes-alist
      '(
        (python-mode . indent-tools-indentation-of-python)
        (yaml-mode . indent-tools-indentation-of-yaml)
        (jade-mode . indent-tools-indentation-of-jade)
       ))

(use-package cuda-mode)
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))

(use-package kconfig-mode)

;; (defun jw-c-common-hook()
;;   (message "hook executed")
  ;; if we have rtags use that
;; (unless (featurep #'rtags)
;;   (message "lsp hook added")
;;      (lambda ()
;;        (add-hook 'c-mode-common-hook 'lsp)));;)


;; (add-hook 'c-mode-common-hook 'jw-c-common-hook)
;; (remove-hook 'c-mode-common-hook 'jw-c-common-hook)
