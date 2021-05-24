;;; -*- lexical-binding: t; -*-


(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir (file-name-directory load-file-name))
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(add-hook 'after-save-hook 
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

(require 'jw-fctn)

(set-face-attribute 'default nil :family "Liberation Mono" :height 145 :weight 'normal)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; tell me what files I am editting
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))
;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq read-quoted-char-radix 16)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1) ;; allow auto reload of externally modified file

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil)

;; Get rid of toolbar, scrollbar, menubar
(progn
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(setq-default tab-width 4)
(set-default 'indent-tabs-mode nil) ;; Indent with spaces not tabs

;; Set up WindMove to be able to move between windows with shift-arrow
(when (fboundp 'windmove-default-keybindings)
	  (windmove-default-keybindings))

;; the default true seems a bit flaked out!
(setq resize-mini-windows nil)

;; doesnt work-
;;(straight-use-package 'ws-trim)
;;(require 'ws-trim)

(use-package ws-butler)
(ws-butler-global-mode t)
;;(global-ws-trim-mode t)

(global-linum-mode 1)
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      fundamental-mode
                                      text-mode
                                      vterm-mode
                                      doc-view-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)

(setq confirm-kill-emacs 'y-or-n-p)

(use-package bind-key)

;; make f9 always open a bash shell
(when (equal window-system 'w32)
  (bind-key "<f9>" #'jw-git-bash-on-windows-shell)
  ;; on windows have ctrl f9 open a dos shell
  (bind-key "<C-f9>" #'jw-new-shell)
  )
(when (not (equal window-system 'w32))
  (bind-key "<f9>" #'jw-new-shell))

;; on winders we need to tell emacs where aspell is and
;; explicitly set the dictionary to british
;; nb there is no aspell that works with emacs 25 on windows
(if (eq system-type 'windows-nt)
    (progn
      (setq ispell-local-dictionary "en_GB")
      (setq ispell-local-dictionary-alist
         '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
           ))

      (setenv "LANG" "en_GB")
      (setq-default ispell-program-name "hunspell")))

(if (eq system-type 'darwin)
    (setq ispell-program-name "/usr/local/bin/aspell"))

(setq kill-buffer-query-functions nil)
(setq confirm-kill-processes nil)

(use-package buffer-move)

(add-hook 'text-mode-hook #'visual-line-mode)

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(when (equal window-system 'w32)
  (setq
   w32-pass-lwindow-to-system nil
   w32-lwindow-modifier 'super
   w32-pass-rwindow-to-system nil
   w32-rwindow-modifier 'super
   w32-pass-apps-to-system nil
   w32-apps-modifier 'hyper
   w32-pass-alt-to-system nil
   w32-scroll-lock-modifier nil))

;; Make scripts executable on Save (saves having to do the chmod every time)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook #'visual-line-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)

(defun do-nothing (interactive))
(defalias 'view-emacs-news 'do-nothing)
(defalias 'describe-gnu-project 'do-nothing)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)
(setq dired-use-ls-dired nil)

;; https://github.com/jocap/emacs.d/blob/master/config/config.org#search--replace-visual-regexp
(use-package pcre2el)

(use-package visual-regexp
	     :defer) ; prevent loading this package before visual-regexp-steroids!

(use-package visual-regexp-steroids
  :demand
  :ensure pcre2el ; much faster than Python
  :config (setq vr/engine 'pcre2el)
  :bind (("<f10>" . #'vr/replace)
         ("<C-f10>" . #'vr/query-replace)))

(require 'jw-bind)
(add-hook 'kill-emacs-hook 'jw-refresh-elc)

(use-package hungry-delete)
(global-hungry-delete-mode)

;; do this last so we have some kind of useful setup in case of failure

;; load jwm*.el modules recursively
;; (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
;;        (modules (directory-files-recursively current-dir "jwm.*\\.el$")))
;;   (mapc 'load modules))

;; sudo apt-get install -y fonts-symbola
;; fc-list :lang=zh-cn

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿 ♕
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
;; todo -- where are my coloured fonts on win10?


;; font sizes
(global-set-key (kbd "C-M-s-.")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))

(global-set-key (kbd "C-M-s-,")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 10)))))


(defun display-startup-echo-area-message ()
  "Try to be helpful in the startup!."
  (message "Alt-f4 to quit, f1 for help."))

(provide 'jw-cmn)
