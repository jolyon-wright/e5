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

(defun jw-sly-make-rainbow-work()
  (setq font-lock-mode 1))

(add-hook 'sly-mrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sly-mrepl-mode-hook 'jw-sly-make-rainbow-work)

(use-package sly-quicklisp :after sly)
(use-package sly-asdf :after sly)

(use-package sly
  ;; :hook ((sly-mrepl-mode . 'rainbow-delimiters-mode))
  ;; :hook
  ;; ((sly-mrepl-mode . (font-lock-mode 1)))
  :config
  (if (eq system-type 'darwin)
      (setq sly-lisp-implementations
            `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))
    (setq sly-lisp-implementations
          `((sbcl ("/usr/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))))


(straight-use-package 'geiser-guile)

;;(use-package scheme48)
;;(setq scheme-program-name "scheme48")
;; slime48 ?

;; (if (eq system-type 'darwin)
;;     ;; brew install mit-scheme
;;     (setq scheme-program-name   "/usr/local/bin/mit-scheme")
;;   )
;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/


;; https://github.com/joaotavora/sly/issues/124
