;;; -*- lexical-binding: t; -*-


(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
 ;; (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))


(setq enable-local-variables :all)

;; (if (eq system-type 'windows-nt)
;;     (progn
;;       (message "all-the-icons is broken on windows - revisit")
;;       (message "treemacs is broken on windows - revisit"))
;;   (progn
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
    (add-hook 'dired-mode-hook 'treemacs-icons-dired-mode);;))

(use-package diminish
  :config (diminish 'eldoc-mode))

(use-package expand-region
             :bind (("C-c n" . er/expand-region)))

(use-package avy
  :bind ("C-c l" . avy-goto-line))

(use-package ivy-avy)

(shut-up (use-package iedit))

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
(bind-key "s-[" #'helm-occur)


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


(bind-key "<C-f1>" #'whitespace-mode)
(bind-key "<C-f2>" #'global-hl-line-mode)

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 1.2))

(use-package amx)
(bind-key "M-x" 'amx)
(bind-key "M-z" 'amx-major-mode-commands)


(use-package package-lint)
(use-package package-lint-flymake)
;; (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)


(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))

(straight-use-package 'lispy)

(global-prettify-symbols-mode t)


(use-package vertico
;; <remap> <backward-paragraph>    vertico-previous-group
;; <remap> <beginning-of-buffer>   vertico-first
;; <remap> <end-of-buffer>         vertico-last
;; <remap> <exit-minibuffer>       vertico-exit
;; <remap> <forward-paragraph>     vertico-next-group
;; <remap> <kill-ring-save>        vertico-save
;; <remap> <minibuffer-beginning-of-buffer>
;;                                 vertico-first
;; <remap> <next-line>             vertico-next
;; <remap> <next-line-or-history-element>
;;                                 vertico-next
;; <remap> <previous-line>         vertico-previous
;; <remap> <previous-line-or-history-element>
;;                                 vertico-previous
;; <remap> <scroll-down-command>   vertico-scroll-down
;; <remap> <scroll-up-command>     vertico-scroll-up



  ;; previous-line-or-history-element

  ;; :ensure t
  ;; :bind (:map vertico-map
  ;;        ("C-j" . vertico-next)
  ;;        ("C-k" . vertico-previous)
  ;;        ("C-f" . vertico-exit)
  ;;        :map minibuffer-local-map
  ;;        ("M-h" . backward-kill-word)
  ;;        )
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  )

(advice-add #'vertico--setup :after
            (lambda (&rest _)
              (setq-local completion-auto-help nil
                          completion-show-inline-help nil)))

;; (define-key vertico-map "?" #'minibuffer-completion-help)
;; (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
;; (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

(require 'recentf)


;; enable recent files mode.
(recentf-mode t)

(bind-key "C-x C-g" 'recentf-open-files)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
;; https://github.com/abo-abo/swiper/issues/1560

(use-package switch-buffer-functions
  :defer t
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  ;; :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :after vertico
  ;; :ensure t
  :custom (completion-styles '(orderless)))


;; https://cestlaz.github.io/post/using-emacs-80-vertico/

;; Example configuration for Consult
(use-package consult
  :after vertico

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)


(use-package embark
;;  :ensure t
  :after vertico

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  ;;:ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))





(defun jw-annotate()
  (interactive)
  (insert
   (concat "JCW:"
           (number-to-string (ts-year (ts-now)))
           "-"
           (format "%02d" (ts-month (ts-now)))
           "-"
           (format "%02d" (ts-day (ts-now)))
           " - ")))

(defvar jw-counter 42)

(require 'cl-lib)

(defun  jw-ins-counter ()
  (interactive)
  (insert (concat "JCW-" (format "%d" (cl-incf jw-counter)) " ")))


(defun jw-get-current-line-number()
  (interactive)
  (kill-new  (format "%d" (line-number-at-pos)))
  )


(bind-keys*
           :prefix "<f8>"
           :prefix-map jw-global-prefix-map
           ("l" . display-line-numbers-mode)
           ("v" . vertico-mode)
           ("c" . company-mode)
           ("u" . cua-mode)
           ("a" . jw-annotate)
           ("n" . jw-get-current-line-number)
           ("i" . jw-ins-counter)
           )
