;;; -*- lexical-binding: t; -*-

(use-package bind-key)
;;(require 'bind-key)

;;(bind-key  "<f10>" 'replace-string)
(bind-key  "<f6>" 'cua-mode)

(bind-key  "s-." 'vr/isearch-forward)
(unbind-key "s-,")
(bind-key  "s-," 'vr/isearch-backward)
(bind-key  "s-[" 'xah-beginning-of-line-or-block)
(bind-key  "s-]" 'xah-end-of-line-or-block)

(bind-key  "<C-f6>" 'linum-mode)

;;(bind-key "<C-M-escape>" 'jw-pull-rebase-branch)
;;(bind-key "<C-escape>" 'jw-clone-branch)
(bind-key "<C-escape>" 'jw-get-lisp)

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

(provide 'jw-bind)

