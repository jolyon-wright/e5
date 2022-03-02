;; https://lisp-lang.org/learn/getting-started/

;; # sudo apt install sbcl
;; curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
;;        --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
;;        --eval '(ql:add-to-init-file)' \
;;        --quit
;; sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

;; interesting https://www.defmacro.org/ramblings/lisp.html
;; https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/
;;
;;https://github.com/norvig/paip-lisp
;;
;; reminder Ctrl C+C transfers to repl

;; (use-package slime
;; 	     :demand t
;; 	     )
(use-package common-lisp-snippets)
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

(use-package rainbow-delimiters
	     :demand t
	     )

;; Warning (emacs): To restore SLIME in this session, customize ‘lisp-mode-hook’
;; and replace ‘sly-editing-mode’ with ‘slime-lisp-mode-hook’.

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sly-editing-mode 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'hy-mode-hook 'rainbow-delimiters-mode)

(defun jw-sly-make-rainbow-work()
  (setq font-lock-mode 1))

(add-hook 'sly-mrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sly-mrepl-mode-hook 'jw-sly-make-rainbow-work)

(use-package sly-quicklisp :after sly)
(use-package sly-asdf :after sly)


(defun op/sly-mrepl (arg)
  "Find or create the first useful REPL for the default connection in a side window."
  (interactive "P")
  (save-excursion
    (sly-mrepl nil))
  (let ((buf (sly-mrepl--find-create (sly-current-connection))))
    (if arg
        (switch-to-buffer buf)
      (pop-to-buffer buf))))

(use-package sly
  :hook ((lisp-mode . prettify-symbols-mode)
         ;;(lisp-mode . op/disable-tabs)
         (lisp-mode . sly-symbol-completion-mode))
  :custom (inferior-lisp-program "sbcl")
  :bind (:map sly-mode-map
              ("C-c C-z" . op/sly-mrepl))
  :config
    (if (eq system-type 'darwin)
      (setq sly-lisp-implementations
            `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))
    (setq sly-lisp-implementations
          `((sbcl ("/usr/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix))))


  (use-package sly-mrepl
    :straight nil  ;; it's part of sly!
    :bind (:map sly-mrepl-mode-map
                ("M-r" . comint-history-isearch-backward))))


(straight-use-package 'geiser-guile)
(setq browse-url-browser-function 'eww-browse-url)

;; hard code for the mo
(setq common-lisp-hyperspec-root
  (concat "file://" (expand-file-name "/Users/jolyon/.emacs.d/straight/repos/e5/scl/HyperSpec/")))

;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/


;; https://github.com/joaotavora/sly/issues/124

;; https://www.racket-mode.com/
;; https://www.linw1995.com/en/blog/Write-Racket-With-Emacs/
(use-package racket-mode
  :ensure t
  :commands racket-mode
  :hook (racket-mode . racket-xp-mode)

  :config
  (setq racket-smart-open-bracket-enable t))

(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "<f5>") 'racket-run)))

(add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
(add-hook 'racket-repl-mode 'rainbow-delimiters-mode)

;; (use-package company
;;   :straight t
;;   :config
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-idle-delay 0.1)
;;   (setq company-tooltip-align-annotations t)
;;   :hook
;;   ((racket-mode . company-mode)
;;    (racket-repl-mode . company-mode)))

(add-hook 'racket-mode-hook 'company-mode)
(add-hook 'racket-repl-mode 'company-mode)

(use-package scribble-mode)
;; (use-package geiser
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq geiser-default-implementation '(racket)))

;; drracket -
;; raco pkg install drcomplete
