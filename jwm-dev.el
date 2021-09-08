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

;; http://ergoemacs.org/emacs/elisp_defvar_problem.html

;; (defvar company-active-map
;;   (let ((keymap (make-sparse-keymap)))
;;     (define-key keymap "\e\e\e" 'company-abort)
;;     (define-key keymap "\C-g" 'company-abort)
;;     (define-key keymap (kbd "M-n") 'company--select-next-and-warn)
;;     (define-key keymap (kbd "M-p") 'company--select-previous-and-warn)
;;     (define-key keymap (kbd "C-n") 'company-select-next-or-abort)
;;     (define-key keymap (kbd "C-p") 'company-select-previous-or-abort)
;;     (define-key keymap (kbd "<down>") 'company-select-next-or-abort)
;;     (define-key keymap (kbd "<up>") 'company-select-previous-or-abort)
;;     (define-key keymap [remap scroll-up-command] 'company-next-page)
;;     (define-key keymap [remap scroll-down-command] 'company-previous-page)
;;     (define-key keymap [down-mouse-1] 'ignore)
;;     (define-key keymap [down-mouse-3] 'ignore)
;;     (define-key keymap [mouse-1] 'company-complete-mouse)
;;     (define-key keymap [mouse-3] 'company-select-mouse)
;;     (define-key keymap [up-mouse-1] 'ignore)
;;     (define-key keymap [up-mouse-3] 'ignore)
;;     ;; (define-key keymap [return] 'company-complete-selection)
;;     ;; (define-key keymap (kbd "RET") 'company-complete-selection)
;;     ;; (define-key keymap [tab] 'company-complete-common)
;;     ;; (define-key keymap (kbd "TAB") 'company-complete-common)
;;     (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
;;     (define-key keymap (kbd "C-h") 'company-show-doc-buffer)
;;     (define-key keymap "\C-w" 'company-show-location)
;;     (define-key keymap "\C-s" 'company-search-candidates)
;;     (define-key keymap "\C-\M-s" 'company-filter-candidates)
;;     (company-keymap--bind-quick-access keymap)
;;      keymap)
;;   "Keymap that is enabled during an active completion. (enhanced by jolyon).")

(straight-use-package company '(jolyon929 :type git :host bitbucket :repo "jolyon929/company-mode")
  ;; (return . company-complete-selection)

  ;; :init (global-company-mode)
  ;; :config
  ;; (setq company-idle-delay .4
  ;;       company-minimum-prefix-length 4)

  ;; :bind
  ;; (:map company-active-map
  ;;       ("<return>" . company-abort)
  ;;       ("<tab>" . company-abort)
  ;;       ("RET"  . company-abort)
  ;;       ("<C-return>" . company-complete-selection)
  ;; ;;      ("TAB"  . company-complete-selection)
  ;; ;;      ("<tab>" . company-complete-selection)
  ;;       )
  )
(bind-key "<C-f3>" #'global-company-mode)
(setq company-global-modes '(not text-mode org-mode))

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
            :foreground "white")
  )

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
