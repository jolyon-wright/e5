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

(use-package slime
	     :demand t
	     )
(use-package common-lisp-snippets)
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(use-package rainbow-delimiters
	     :demand t
	     )

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

;;(use-package scheme48)
;;(setq scheme-program-name "scheme48")
;; slime48 ?

(if (eq system-type 'darwin)
    ;; brew install mit-scheme
    (setq scheme-program-name   "/usr/local/bin/mit-scheme")
)
;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
