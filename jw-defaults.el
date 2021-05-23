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
 case-fold-search nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq-default tab-width 4)
(set-default 'indent-tabs-mode nil) ;; Indent with spaces not tabs

;; Set up WindMove to be able to move between windows with shift-arrow
(when (fboundp 'windmove-default-keybindings)
	  (windmove-default-keybindings))

;; the default true seems a bit flaked out!

(use-package bind-key)
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
                (save-excursion (byte-compile-file buffer-file-name)))))

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

(straight-use-package '(revive :type git :host github :repo "vedang/revive-mode")
                      :init
                      (progn
                        (add-to-list 'load-path (straight--repos-dir "revive-mode"))
                        (require 'revive-mode-config))
                      :config
                      (progn
                        ;; save and restore layout
                        (add-hook 'kill-emacs-hook 'emacs-save-layout)
                        (add-hook 'after-init-hook 'emacs-load-layout t)))




(provide 'jw-defaults)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jw-defaults.el ends here.
