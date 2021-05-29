;;; -*- lexical-binding: t; -*-

(use-package cmake-mode :defer)
(use-package dtrt-indent :defer)

(setq clang-format-style-option "google")

;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir (file-name-directory load-file-name))
;;            (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))

(require 'jw-google-c-style)

;; (sp-local-pair 'c-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))

;;(require 'cmake-mode)
(use-package cmake-mode
  :defer)

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

(use-package company
  ;; (return . company-complete-selection)

  :init (global-company-mode)
  :bind
  (:map company-active-map
        ("<return>" . nil)
        ("<tab>" . nil)
        ("RET"  . nil)
        ("TAB"  . company-complete-selection)
        ("<tab>" . company-complete-selection)
        )
  )
(bind-key "<C-f3>" #'global-company-mode)

(cmake-ide-setup)

;; (global-company-mode)


;;(if (eq system-type 'windows-nt)
;; (if (nil)
;;     (message "rtags is broken on windows - revisit")
;;   (progn
;;     (use-package rtags)
;;     (add-to-list 'load-path "~/.emacs.d/straight/repos/rtags/src")

;;     (require 'company-rtags)


;; ;;!    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;     ;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

;;     (setq rtags-display-result-backend 'helm)

;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)


;;     ;;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;     ;; (define-key c-mode-base-map (kbd "<backtab>") (function company-complete))
;;     (setq rtags-use-helm t)


;;     ;; ensure that we use only rtags checking
;;     ;; https://github.com/Andersbakken/rtags#optional-1
;;     (defun setup-flycheck-rtags ()
;;       (interactive)
;;       (flycheck-select-checker 'rtags)
;;       ;; RTags creates more accurate overlays.
;;       (setq-local flycheck-highlighting-mode nil)
;;       (setq-local flycheck-check-syntax-automatically nil))

;;     ;; only run this if rtags is installed
;;     (when (require 'rtags nil :noerror)
;;       ;; make sure you have company-mode installed
;;       (require 'company)
;;       ;; (define-key c-mode-base-map (kbd "M-.")
;;       ;;   (function rtags-find-symbol-at-point))
;;       ;; (define-key c-mode-base-map (kbd "M-,")
;;       ;;   (function rtags-find-references-at-point))
;;       ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;;       ;;  (define-key prelude-mode-map (kbd "C-c r") nil)
;;       ;; install standard rtags keybindings. Do M-. on the symbol below to
;;       ;; jump to definition and see the keybindings.
;;       (rtags-enable-standard-keybindings)
;;       ;; comment this out if you don't have or don't use helm
;;       (setq rtags-use-helm t)
;;       ;; company completion setup
;;       (setq rtags-autostart-diagnostics t)
;;       (rtags-diagnostics)
;;       (setq rtags-completions-enabled t)
;;       (push 'company-rtags company-backends)
;;       (global-company-mode)
;;       ;;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

;;       ;; (define-key c-mode-base-map (kbd "<s-f11>") (function company-complete))

;;       ;; use rtags flycheck mode -- clang warnings shown inline
;;       (require 'flycheck-rtags)
;;       ;; c-mode-common-hook is also called by c++-mode
;;       (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)

;;       (use-package rtags-xref)
;;       (add-hook 'c-mode-common-hook 'rtags-xref-enable)
;;       )
;;     )
;;   )

(bind-key "<f7>" 'compile)
(bind-key "<C-f8>" 'align)
(bind-key "<f5>" 'gdb)
(bind-key "<C-f5>" 'jw-gdb-other-frame)

