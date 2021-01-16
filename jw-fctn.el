;;; -*- lexical-binding: t; -*-

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

(defun jw-new-shell ()
  (interactive)
  (let (
        (currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*"))
        )
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(defun jw-git-bash-on-windows-shell ()
  ;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
  ;;   if [ -n "$INSIDE_EMACS" ]; then
  ;;     export PS1='\[\033[32m\]\u@\h \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ '
  ;; fi

  ;; from within shell

  ;;eval `ssh-agent -s`
  ;;ssh-add ~/.ssh/*_rsa

  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (setq explicit-bash.exe-args '("--login" "-i"))
    ;; make sure we use the "right" bash!
    (let ((explicit-shell-file-name "c:/Program Files/Git/bin/bash.exe"))
      (shell newbuf))))

(defun jw-select-line()
  "select the current line"
  (interactive)
  (let (b1)
    (beginning-of-line)
    (setq b1 (point))
    (set-mark b1)
    (end-of-line)))

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

(defun upcase-char (arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))

(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))

(defun jw-switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jw-switch-to-Messages-buffer ()
  "Switch to the current session's Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun get-key-combo (key)
  "Just return the key combo entered by the user"
  (interactive "kKey combo: ")
  key)

(defun keymap-unset-key (key keymap)
  "Remove binding of KEY in a keymap KEY is a string or vector
representing a sequence of keystrokes."
  (interactive
   (list (call-interactively #'get-key-combo)
         (completing-read "Which map: " minor-mode-map-alist nil t)))
  (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
    (when map
      (define-key map key nil)
      (message  "%s unbound for %s" key keymap))))

(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i)))))

(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-end-of-line-or-block (&optional n)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-beginning-of-line-or-block )
                )
            (xah-forward-block)
          (end-of-line))
      (progn (xah-forward-block n)))))

(defun xah-beginning-of-line-or-block (&optional n)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (xah-backward-block n)
          (beginning-of-line))
      (xah-backward-block n))))


;; (use-package git)
;; (defun jw-pull-rebase-branch (&optional branch)
;;     "Do a pull --rebase"
;;     (interactive)
;;     (unless branch
;;       (setq branch (read-string "which branch to rebase onto the current branch?")))
;;     (let ((git-repo (expand-file-name
;;                      "lisp/" user-emacs-directory)))
;;       (message (concat "pulling " branch))
;;       (string-trim
;;        (git-run "pull"
;;                 "--rebase"
;;                 "origin"
;;                 branch))
;;       (message (concat branch " pulled.")))
;;     )


(defun jw-refresh-elc ()
  ;; byte compile
  (require 'bytecomp)

  (byte-recompile-file (expand-file-name "init.el" user-emacs-directory) nil 0)
  (byte-recompile-directory (expand-file-name "straight/repos/e5" user-emacs-directory)  0 nil)

  (if (not (version< emacs-version "28.0"))
      (native-compile-async (expand-file-name "straight/repos/e5" user-emacs-directory) 'recursively)))

(defun jw-get-fullpathdir ()
  "get the qualified path of this script"
  (file-name-directory (or load-file-name buffer-file-name)))

(defun make-sub-dir-name(s)
  "get the qualified subdir of s"
  (concat (jw-get-fullpathdir) s))

(provide 'jw-fctn)
