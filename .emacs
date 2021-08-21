;;; minimal_dot_emacs.el --- Description    -*- lexical-binding: t; -*-

;; Author: build  build@gvc3-test-pc
;; Keywords:
;; URL:

;; Copyright (C) 2021, build, all rights reserved.

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
; basic usability
(fset 'yes-or-no-p 'y-or-n-p)  ; Ask for y/n instead of yes/no
(setq confirm-kill-processes nil)
(desktop-save-mode 1)
(setq inhibit-startup-message t)
(load-theme 'wombat)
(global-linum-mode t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(define-coding-system-alias 'UTF-8 'utf-8)

; don't have the cursor blink in any buffer
(setq blink-cursor-mode nil)

; default text size + resizing text
;; (set-face-attribute 'default nil :height 140)
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase 1)))

(global-set-key (kbd "M-<up>") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (text-scale-decrease 1)))

; don't ask to kill shells
(defun set-no-process-query-on-exit ()
 (let ((proc (get-buffer-process (current-buffer))))
   (when (processp proc)
     (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)
(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)

; shell creation
(defun new-shell ()
  (interactive)

  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*")))

   (generate-new-buffer newbuf)
   (set-window-dedicated-p currentbuf nil)
   (set-window-buffer currentbuf newbuf)
   (shell newbuf)))

(defun select-line () (interactive) (progn (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)))

(global-set-key (kbd "<f9>") 'new-shell)
(global-set-key (kbd "M-9") 'select-line)

; switching windows
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings))

; resizing windows
(global-set-key (kbd "M-s-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<up>") 'enlarge-window)
(global-set-key (kbd "M-s-<down>") 'shrink-window)

; xref jump to definition
(global-set-key (kbd "M-.") 'xref-find-definitions)

; c-mode use google c style with 4 space default indentation.
;; (add-to-list 'load-path (concat user-emacs-directory "lisp"))
;; (require 'google-c-style)
;; (add-hook 'c-mode-hook 'google-set-c-style)

; c-mode (, {, [, ', " auto-completion.
(add-hook 'c-mode-hook 'electric-pair-local-mode)

(setq-default tab-width 4)
(set-default 'indent-tabs-mode nil) ;; Indent with spaces not tabs

(global-auto-revert-mode 1) ;; allow auto reload of externally modified file

(defun jw-file-name-copy-path ()
  "Copy the path the of the of the current buffer"
  (interactive)
  (kill-new (message "%s" (buffer-file-name))))

(global-set-key (kbd "C-§")  'jw-file-name-copy-path)
(global-set-key (kbd "C-`")  'jw-file-name-copy-path)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(set-face-attribute 'default nil :family "Liberation Mono" :height 145 :weight 'normal)

;; Local Variables:
;; coding: utf-8
;; End:



;;; minimal_dot_emacs.el ends here.
