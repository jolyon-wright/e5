;;; jw-defaults.el --- sane defaults    -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'map)
(require 'subr-x)


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
(show-paren-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1) ;; allow auto reload of externally modified file

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)
(setq
 make-backup-files nil
 load-prefer-newer t
 auto-save-default nil
 read-quoted-char-radix 16
 vc-follow-symlinks t
 inhibit-startup-screen t
 initial-scratch-message nil
 sentence-end-double-space nil
 ring-bell-function 'ignore
 use-dialog-box nil
 mark-even-if-inactive nil
 kill-whole-line t
 resize-mini-windows nil
 confirm-kill-emacs 'y-or-n-p
 kill-buffer-query-functions nil
 confirm-kill-processes nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 custom-file (make-temp-file "")
 custom-safe-themes t
 dired-use-ls-dired nil
 case-fold-search nil
 blink-cursor-mode nil)

(global-linum-mode t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)



(setq-default tab-width 4)
(set-default 'indent-tabs-mode nil) ;; Indent with spaces not tabs

;; Set up WindMove to be able to move between windows with shift-arrow
(when (fboundp 'windmove-default-keybindings)
	  (windmove-default-keybindings))

;; the default true seems a bit flaked out!

(use-package bind-key)
(require 'bind-key)
(use-package buffer-move)

(defun do-nothing (interactive))
(defalias 'view-emacs-news 'do-nothing)
(defalias 'describe-gnu-project 'do-nothing)

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))

(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))

(add-hook 'dired-mode-hook #'my-dired-mode-hook)

;; https://github.com/jocap/emacs.d/blob/master/config/config.org#search--replace-visual-regexp
(use-package pcre2el)

(use-package visual-regexp
	     :defer) ; prevent loading this package before visual-regexp-steroids!

(use-package visual-regexp-steroids
  :demand t
  :ensure pcre2el ; much faster than Python
  :config (setq vr/engine 'pcre2el)
  :bind (("<f10>" . #'vr/replace)
         ("<C-f10>" . #'vr/query-replace)))

(use-package hungry-delete)
(global-hungry-delete-mode)

;; sudo apt-get install -y fonts-symbola
;; fc-list :lang=zh-cn

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿 ♕
;; (set-fontset-font t 'symbol "Apple Color Emoji")
;; (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;; (set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package emojify
  :demand t
  :init (setq emojify-download-emojis-p t)
  :config (global-emojify-mode))

;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ;;    ((member "Twitter Color Emoji" (font-family-list)) "Twitter Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")))

(defun display-startup-echo-area-message ()
  "Try to be helpful in the startup!."
  (message "Alt-f4 to quit, f1 for help."))

(bind-key "<M-f4>" #'save-buffers-kill-terminal)

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (shut-up (save-excursion (byte-compile-file buffer-file-name))))))

(use-package flycheck)
(add-hook 'sh-mode-hook 'flycheck-mode)

(use-package auto-capitalize
  :demand t
  :config
  (progn
    (setq
     sentence-end "[.?!][]\"')}]*\\($\\|     \\|  \\)[
]*" ;; um... use ".  " as sentence end. prevents auto-cap from getting confused
     ))
  :bind (:map text-mode-map
              ("M-." . nil)
              ("M-." . (lambda() (interactive)(insert ".  ")))
              ("C-s-<right>" . (lambda()
                                 (interactive)
                                 (end-of-line)
                                 (insert " ")))
              ;; add a space where I always put one
              ("," . (lambda() (interactive)(insert ", ")))
              (";" . (lambda() (interactive)(insert "; ")))
              ("?" . (lambda() (interactive)(insert "? ")))))

(add-hook 'text-mode-hook 'turn-on-auto-capitalize-mode)

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

(defmacro radian--with-silent-write (&rest body)
  "Execute BODY, with the function `write-region' made silent."
  (declare (indent 0))
  `(radian-flet ((defun write-region
                     (start end filename &optional append visit lockname
                            mustbenew)
                   (funcall write-region start end filename append 0
                            lockname mustbenew)
                   (when (or (stringp visit) (eq visit t))
                     (setq buffer-file-name
                           (if (stringp visit)
                               visit
                             filename))
                     (set-visited-file-modtime)
                     (set-buffer-modified-p nil))))
     (cl-letf (((symbol-function #'message) #'ignore))
       ,@body)))

;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(use-package saveplace
  :demand t
  :config

  (save-place-mode +1)

  (radian-defadvice radian--advice-save-place-quickly-and-silently
      (func &rest args)
    :around #'save-place-alist-to-file
    "Make `save-place' save more quickly and silently."
    (radian--with-silent-write
      (cl-letf (((symbol-function #'pp) #'prin1))
        (apply func args)))))

(straight-use-package '(revive :type git :host github :repo "vedang/revive-mode"))
;; oh the horror of it all

 
;; (straight-use-package '(revive :type git :host github :repo "vedang/revive-mode")
;;                       :init
;;                       (progn
;;                         (add-to-list 'load-path (straight--repos-dir "revive-mode"))
;;                         (require 'revive-mode-config))
;;                       :config
;;                       (progn
;;                         ;; save and restore layout
;;                         (add-hook 'kill-emacs-hook 'emacs-save-layout)
;;                         (add-hook 'after-init-hook 'emacs-load-layout t)))

(bind-key "<M-up>"           'text-scale-increase)
(bind-key "<M-down>"         'text-scale-decrease)
(bind-key "<M-s-up>"         'enlarge-window)
(bind-key "<M-s-down>"		 'shrink-window)
(bind-key "<M-s-left>"		 'enlarge-window-horizontally)
(bind-key "<M-s-right>"		 'shrink-window-horizontally)

(bind-key "<C-M-s-up>"       'enlarge-window)
(bind-key "<C-M-s-down>"	 'shrink-window)
(bind-key "<C-M-s-left>"	 'enlarge-window-horizontally)
(bind-key "<C-M-s-right>"	 'shrink-window-horizontally)

;; absent # key on the mac
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))

(use-package multi-shell)
(bind-key "<f9>" #'multi-shell-new)

(use-package flyspell-popup
  :init (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  )

;;https://spwhitton.name//blog/entry/transient-caps-lock/
(defun spw/transient-caps-self-insert (&optional n)
  (interactive "p")
  (insert-char (upcase last-command-event) n))

(defun spw/activate-transient-caps ()
  "Activate caps lock while typing the current whitespace-delimited word(s).
This is useful for typing Lisp symbols and C enums which consist
of several all-uppercase words separated by hyphens and
underscores, such that M-- M-u after typing will not upcase the
whole thing."
  (interactive)
  (let* ((map (make-sparse-keymap))
         (deletion-commands '(delete-backward-char
                              paredit-backward-delete
                              backward-kill-word
                              paredit-backward-kill-word
                              spw/unix-word-rubout
                              spw/paredit-unix-word-rubout))
         (typing-commands (cons 'spw/transient-caps-self-insert
                                deletion-commands)))
    (substitute-key-definition 'self-insert-command
                               #'spw/transient-caps-self-insert
                               map
                               (current-global-map))
    (set-transient-map
     map
     (lambda ()
       ;; try to determine whether we are probably still about to try to type
       ;; something in all-uppercase
       (and (member this-command typing-commands)
            (not (and (eq this-command 'spw/transient-caps-self-insert)
                      (= (char-syntax last-command-event) ?\ )))
            (not (and (or (bolp) (= (char-syntax (char-before)) ?\ ))
                      (member this-command deletion-commands))))))))

;; (global-set-key "\M-C" #'spw/activate-transient-caps)
(bind-key "s-/"	 'spw/activate-transient-caps)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq resize-mini-windows t)


(defun jw-get-fullpathdir ()
  "get the qualified path of this script"
  (file-name-directory (or load-file-name buffer-file-name)))

(defun jw-make-sub-dir-name(s)
  "get the qualified subdir of s"
  (concat (jw-get-fullpathdir) s))

(defun jw-select-line()
  "select the current line"
  (interactive)
  (let (b1)
    (beginning-of-line)
    (setq b1 (point))
    (set-mark b1)
    (end-of-line)))

;; Parse tree walker
; by Nikolaj Schumacher, 2008-10-20. Licensed under GPL.
(defun jw-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun jw-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
             (or (and transient-mark-mode mark-active)
             (eq last-command this-command))))
  (if incremental
      (progn
        (jw-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (jw-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))


;; josh moller-mara
(defun jw-file-name-copy-path ()
  "Copy the path the of the of the current buffer"
  (interactive)
  (kill-new (message "%s" (buffer-file-name))))

;; Kill the current buffer immediatly, saving it if needed.
;; https://stackoverflow.com/questions/6467002/how-to-kill-buffer-in-emacs-without-answering-confirmation
(defvar jw-kill-save-buffer-delete-windows t
  "*Delete windows when `kill-save-buffer' is used.
If this is non-nil, then `kill-save-buffer' will also delete the corresponding
windows.  This is inverted by `kill-save-buffer' when called with a prefix.")

(defun jw-kill-save-buffer (arg)
  "Save the current buffer (if needed) and then kill it.
Also, delete its windows according to `jw-kill-save-buffer-delete-windows'.
A prefix argument ARG reverses this behavior."
  (interactive "P")
  (let ((del jw-kill-save-buffer-delete-windows))
    (when arg (setq del (not del)))
    (when (and (buffer-file-name) (not (file-directory-p (buffer-file-name))))
      (save-buffer))
    (let ((buf (current-buffer)))
      (when del (delete-windows-on buf))
      (kill-buffer buf))))

(defun jw-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun jw-switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jw-switch-to-Messages-buffer ()
  "Switch to the current session's Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))


(bind-key "<C-tab>"          'other-frame)

(unbind-key "<f3>")

(bind-key "C-§"  #'jw-file-name-copy-path)
(bind-key "C-`"  #'jw-file-name-copy-path)

; select current word
(bind-key  "M-8" #'jw-extend-selection)

; select current line
(bind-key  "M-9" #'jw-select-line)


;; Disabled confused commands
(unbind-key "C-z")                  ; suspend-frame
(unbind-key "C-x C-z")                  ; suspend-frame
(unbind-key "C-x m")			; compose-mail

(bind-key "C-s-c" #'copy-to-register)
(bind-key "C-s-v" #'insert-register)
(bind-key "<C-f12>" #'jw-kill-save-buffer)
(bind-key "<kp-left>" #'buf-move-left)
(bind-key "<kp-right>" #'buf-move-right)
(bind-key "<kp-up>" #'buf-move-up)
(bind-key "<kp-down>" #'buf-move-down)

(bind-key "<s-left>" #'buf-move-left)
(bind-key "<s-right>" #'buf-move-right)
(bind-key "<s-up>" #'buf-move-up)
(bind-key "<s-down>" #'buf-move-down)

(bind-key  "<C-s-up>" 'upcase-char)
(bind-key  "<C-s-down>" 'downcase-char)
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust

;; absent # key on the mac
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))

(unbind-key "s-t")
(unbind-key "<f4>")
(bind-key "<f4>" #'jw-switch-to-scratch-buffer)
(bind-key "<f12>" #'jw-switch-to-Messages-buffer)

;; C-x 4 .         xref-find-definitions-other-window

(bind-key "M-]" 'xref-find-definitions-other-window)
(bind-keys :map emacs-lisp-mode-map
           :prefix "<f3>"
           :prefix-map jw-lisp-prefix-map
           ("d" . eval-defun)
           ("b" . eval-buffer)
           ("e" . eval-last-sexp)
           )

(defadvice find-file (around find-file-line-number
                                        (filename &optional wildcards)
                                        activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(bind-key  "<f6>" 'cua-mode)
(bind-key  "<C-f6>" 'linum-mode)

(defun jw-add-dirs-to-load-path (dir)
  "add dir and all subdirs to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun jw-setup-lisp-remote (dir)
  (interactive "sWhich branch?")
  (let ((default-directory dir))
    (shell-command "git remote add gorigin git@bitbucket.org:jolyon929/e5.git")))

(defun jw-setup-lisp-pull (dir)
  (interactive "sWhich branch?")
  (let ((default-directory dir))
    (shell-command "git pull")))

(defun jw-setup-lisp-dir (dir)
  "set up a new directory of lispology"
  (let ((modules (directory-files-recursively dir "jwm.*\\.el$")))
    (jw-add-dirs-to-load-path dir)
    (mapc (lambda(m) (load m nil t)) modules)
    (shut-up (byte-recompile-directory dir 0 nil))
    (when (fboundp 'native-compile-async) ;; only in e28 with --native-compile
      (shut-up (native-compile-async dir 'recursively)))))

(use-package ws-butler)
(ws-butler-global-mode t)

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


(add-hook 'text-mode-hook #'visual-line-mode)
(bind-key "<C-escape>" 'jw-get-lisp)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))


;; Emacs instances started outside the terminal do not pick up ssh-agent information unless we use keychain-environment. Note to self: if you keep having to enter your keychain password on macOS, make sure this is in .ssh/config:

;; Host *
;;   UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))


(provide 'jw-defaults)

;; gpg/ git-crypt
;; https://www.bytedude.com/gpg-in-emacs/
;; https://dev.to/heroku/how-to-manage-your-secrets-with-git-crypt-56ih
;; https://blog.francium.tech/secure-your-credentials-using-git-crypt-1ccbacc483c7

;; Local Variables:
;; coding: utf-8
;; End:

;;; jw-defaults.el ends here.
