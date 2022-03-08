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


;; raco pkg install rtmidi


;; jolyon@jolyon-XPS-13-9370:~/.emacs.d/straight/repos/e5/scl$ racket
;; Welcome to Racket v8.4 [cs].
;; >     (collection-path "rtmidi")
;; #<path:/home/jolyon/.local/share/racket/8.4/pkgs/rtmidi/rtmidi>
;; > (quit)
;; ; quit: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: top-level
;; ; [,bt for context]
;; >   C-c C-c; user break [,bt for context]
;; > (exit)
;; jolyon@jolyon-XPS-13-9370:~/.emacs.d/straight/repos/e5/scl$ cd /home/jolyon/.local/share/racket/8.4/pkgs/rtmidi/rtmidi
;; jolyon@jolyon-XPS-13-9370:~/.local/share/racket/8.4/pkgs/rtmidi/rtmidi$

;; wget http://www.music.mcgill.ca/~gary/rtmidi/release/rtmidi-2.1.0.tar.gz
;; --2022-03-03 13:13:12--  http://www.music.mcgill.ca/~gary/rtmidi/release/rtmidi-2.1.0.tar.gz
;; Resolving www.music.mcgill.ca (www.music.mcgill.ca)... 132.206.143.14
;; Connecting to www.music.mcgill.ca (www.music.mcgill.ca)|132.206.143.14|:80... connected.
;; HTTP request sent, awaiting response... 200 OK
;; Length: 242064 (236K) [application/x-gzip]
;; Saving to: ‘rtmidi-2.1.0.tar.gz’

;; rtmidi-2.1.0.tar.gz                  100%[=====================================================================>] 236.39K   393KB/s    in 0.6s

;; 2022-03-03 13:13:14 (393 KB/s) - ‘rtmidi-2.1.0.tar.gz’ saved [242064/242064]

;; sudo apt install libasound2-dev
;; tar xvf ./rtmidi-2.1.0.tar.gz
;; make linux


;; raco pkg install rs



;; https://github.com/mcdejonge/rs

;; utils:-
;; raco pkg install rs-l


;; examples:-
;; https://github.com/mcdejonge/rs-demos

;; build rtmidi

;; ./configure --build=aarch64-unknown-linux-gnu
;; sudo apt install librtmidi-dev


;; /home/jolyon/.racket/6.11/pkgs/rtmidi/rtmidi

;; g++ -I /usr/include/rtmidi main.cpp /usr/lib/aarch64-linux-gnu/librtmidi.so

;; amidi -l
;; Dir Device    Name
;; IO  hw:3,0,0  TD-3 MIDI 1
;; jolyon@xav:~/.emacs.d/straight/repos/e5/scl$


;; For anyone else trying, I followed these instructions to rebuild the kernel, with the ALSA sequencer module enabled:


;; sudo apt install qjackctl


;; /home/jolyon/src/knl/kernel/kernel-4.9/sound/core/seq/Kconfig

;; added to defconfig

;; CONFIG_SND_RAWMIDI_SEQ=y
;; CONFIG_SND_SEQUENCER=y
