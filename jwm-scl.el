;; https://lisp-lang.org/learn/getting-started/

;; # sudo apt install sbcl
;; curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
;;        --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
;;        --eval '(ql:add-to-init-file)' \
;;        --quit
;; sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

(use-package slime)
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
