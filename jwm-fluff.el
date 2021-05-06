;;; -*- lexical-binding: t; -*-


(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
 ;; (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))


(setq enable-local-variables :all)

(if (eq system-type 'windows-nt)
    (progn
      (message "all-the-icons is broken on windows - revisit")
      (message "treemacs is broken on windows - revisit"))
  (progn
    (use-package all-the-icons)
    (use-package treemacs
      :defer t
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (progn
        (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
              treemacs-deferred-git-apply-delay      0.5
              treemacs-directory-name-transformer    #'identity
              treemacs-display-in-side-window        t
              treemacs-eldoc-display                 t
              treemacs-file-event-delay              5000
              treemacs-file-extension-regex          treemacs-last-period-regex-value
              treemacs-file-follow-delay             0.2
              treemacs-file-name-transformer         #'identity
              treemacs-follow-after-init             t
              treemacs-git-command-pipe              ""
              treemacs-goto-tag-strategy             'refetch-index
              treemacs-indentation                   2
              treemacs-indentation-string            " "
              treemacs-is-never-other-window         nil
              treemacs-max-git-entries               5000
              treemacs-missing-project-action        'ask
              treemacs-move-forward-on-expand        nil
              treemacs-no-png-images                 nil
              treemacs-no-delete-other-windows       t
              treemacs-project-follow-cleanup        nil
              treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
              treemacs-position                      'left
              treemacs-read-string-input             'from-child-frame
              treemacs-recenter-distance             0.1
              treemacs-recenter-after-file-follow    nil
              treemacs-recenter-after-tag-follow     nil
              treemacs-recenter-after-project-jump   'always
              treemacs-recenter-after-project-expand 'on-distance
              treemacs-show-cursor                   nil
              treemacs-show-hidden-files             t
              treemacs-silent-filewatch              nil
              treemacs-silent-refresh                nil
              treemacs-sorting                       'alphabetic-asc
              treemacs-space-between-root-nodes      t
              treemacs-tag-follow-cleanup            t
              treemacs-tag-follow-delay              1.5
              treemacs-user-mode-line-format         nil
              treemacs-user-header-line-format       nil
              treemacs-width                         35
              treemacs-workspace-switch-cleanup      nil)

        ;; The default width and height of the icons is 22 pixels. If you are
        ;; using a Hi-DPI display, uncomment this to double the icon size.
        ;;(treemacs-resize-icons 44)

        (treemacs-follow-mode t)
        (treemacs-filewatch-mode t)
        (treemacs-fringe-indicator-mode 'always)
        (pcase (cons (not (null (executable-find "git")))
                     (not (null treemacs-python-executable)))
          (`(t . t)
           (treemacs-git-mode 'deferred))
          (`(t . _)
           (treemacs-git-mode 'simple))))
      :bind
      (:map global-map
            ("M-0"       . treemacs-select-window)
            ("C-x t 1"   . treemacs-delete-other-windows)
            ("C-x t t"   . treemacs)
            ("C-x t B"   . treemacs-bookmark)
            ("C-x t C-t" . treemacs-find-file)
            ("C-x t M-t" . treemacs-find-tag)))

    ;; (use-package treemacs-evil
    ;;   :after (treemacs evil)
    ;;   )

    (use-package treemacs-projectile
      :after (treemacs projectile))

    (use-package treemacs-icons-dired
      :after (treemacs dired)
      
      :config (treemacs-icons-dired-mode))

    (use-package treemacs-magit
      :after (treemacs magit))

    (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
      :after (treemacs persp-mode) ;;or perspective vs. persp-mode
      :config (treemacs-set-scope-type 'Perspectives))
    (add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)))

(use-package diminish
  :config (diminish 'eldoc-mode))


(use-package expand-region
             :bind (("C-c n" . er/expand-region)))

(use-package avy
  :bind ("C-c l" . avy-goto-line))

(use-package ivy-avy)

(use-package iedit)

(use-package duplicate-thing
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-c u" . my-duplicate-thing)
         ("C-c C-u" . my-duplicate-thing)))


(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-setup-minibuffer))

(use-package deadgrep
  :bind (("C-c h" . #'deadgrep)))

(use-package symbol-overlay
  :bind ("M-i" . #'symbol-overlay-put)
  :bind ("M-n" . #'symbol-overlay-switch-forward)
  :bind ("M-p" . #'symbol-overlay-switch-backward)
  :bind ("M-o" . #'symbol-overlay-mode)
  )

(bind-key "<C-M-return>" #'gif-screencast)

(use-package gif-screencast
  :config
  (if (eq system-type 'darwin)
      (progn
      ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
      (setq gif-screencast-args '("-x"))
      ;; Optional: Used to crop the capture to the Emacs frame.
      (setq gif-screencast-cropping-program "mogrify")
      ;; Optional: Required to crop captured images.
      (setq gif-screencast-capture-format "ppm"))
      (advice-add
       #'gif-screencast--cropping-region
       :around
       (lambda (oldfun &rest r)
         (apply #'format "%dx%d+%d+%d"
                (mapcar
                 (lambda (x) (* 2 (string-to-number x)))
                 (split-string (apply oldfun r) "[+x]")))))
      )

  :bind (:map gif-screencast-mode-map
              ("<s-return>" . #'gif-screencast-toggle-pause)
              ("M-RET" . #'gif-screencast-stop)
              )
  )

; brew install gifsicle

(use-package helm
  ;;todo -- revisit this

  :bind (:map helm-map
              ("TAB" . helm-maybe-exit-minibuffer)
              ("RET" . helm-select-action)))

;;(require 'helm-config)
(add-hook
 'after-init-hook
 (lambda ()
   (require 'helm-config)
   ;; (helm-mode 1)
   ))
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(bind-key "M-[" #'helm-M-x)

(eval-after-load 'helm
    (lambda ()
      (set-face-attribute 'helm-source-header nil
                          :background "black"
                          :foreground "gray54")))

(use-package hydra
    :config
    :init
      (defhydra hydra-multiple-cursors (:color blue :hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this :exit nil)
  ("N" mc/skip-to-next-like-this :exit nil)
  ("M-n" mc/unmark-next-like-this :exit nil)
  ("p" mc/mark-previous-like-this :exit nil)
  ("P" mc/skip-to-previous-like-this :exit nil)
  ("M-p" mc/unmark-previous-like-this :exit nil)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click :exit nil)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))
      (global-set-key (kbd "C-c r") 'hydra-multiple-cursors/body))

; reminder - list-faces-display

(straight-use-package 'fringe-helper)
(straight-use-package 'git-gutter-fringe+)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
; whitespace mode configuration

; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
  (spaces tabs newline tab-mark newline-mark face lines-tail)))
(setq show-trailing-whitespace 't)

;;(global-whitespace-mode 't)

;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne, ¶
   (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
))


;; colour themes search path
(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))

(bind-key "<C-f1>" #'whitespace-mode)
(bind-key "<C-f2>" #'global-hl-line-mode)

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 1.2))



;; Emacs instances started outside the terminal do not pick up ssh-agent information unless we use keychain-environment. Note to self: if you keep having to enter your keychain password on macOS, make sure this is in .ssh/config:

;; Host *
;;   UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))


;; (use-package smex
;;   )
;; (bind-key "M-x" 'smex)
;; (bind-key "M-z" 'smex-major-mode-commands)


(use-package amx)
(bind-key "M-x" 'amx)
(bind-key "M-z" 'amx-major-mode-commands)

;; (use-package yasnippet
;;   :config
;;   (use-package yasnippet-snippets
;;     )
;;   (yas-global-mode t)
;;   (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;   (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)

;; ;;  (add-to-list #'yas-snippet-dirs (concat user-emacs-directory "lisp/snippets"))


;;   (add-to-list #'yas-snippet-dirs (concat (file-name-directory (or load-file-name buffer-file-name)) "/snippets")  )

;;   (yas-reload-all)
;;   (setq yas-prompt-functions '(yas-ido-prompt))
;;   (defun help/yas-after-exit-snippet-hook-fn ()
;;     (prettify-symbols-mode)
;;     (prettify-symbols-mode))
;;   (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
;;   :diminish yas-minor-mode)


(use-package package-lint)
(use-package package-lint-flymake)
;; (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)


(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))


(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))


(straight-use-package 'lispy)
;; ;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'eval-defun)
;; (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-last-sexp)

(global-prettify-symbols-mode t)
