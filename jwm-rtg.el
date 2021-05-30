;;;; .emacs-rtags -*- lexical-binding: t; -*-

;; on ubuntu 1804:-
;;
;; sudo apt-get install -y libclang-9-dev clang-9 libssl-dev bear cmake zlib1g-dev
;; sudo ln -s /usr/bin/clang-9 /usr/bin/clang
;;
;; then:-
;;
;; copy this file to ~/.emacs.d/init.el
;;
;; emacs &
;;   ;; this will pull the lisp into /home/$USER/.emacs.d
;;
;; then:-
;;
;; cd /home/$USER/.emacs.d/straight/repos/rtags
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
;; make -j8
;; sudo make install
;;
;;   ;; this will compile The Stuff
;;
;; then:-
;;
;; emacs -q --load ~/.emacs-rtags
;;
;;   ;; this will load this config without monkeying with yours

;; two things:-
;;
;;
;; thing(1)
;;
;; if using cmake add this to CMakeLists.txt:-
;;        set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
;;
;; thing(2)
;;
;; if using make do this:-
;;         make clean
;;         bear make
;;         make


;; setup package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; gives us completion
(use-package company
  :config
  (global-company-mode)

  :diminish company-mode)

;; gives us clang indexing
(use-package rtags
  :after (company)	     
  :init
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  :config
  (progn
    (rtags-enable-standard-keybindings)
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t))

  (use-package company-rtags
    :init
    (push 'company-rtags company-backends))

  (use-package helm-rtags
    :init
    (setq rtags-use-helm t)

    :config
    (setq rtags-display-result-backend 'helm))
  
  (use-package rtags-xref)
  (add-hook 'c-mode-common-hook 'rtags-xref-enable)
  
  (use-package flycheck-rtags))

(bind-key "M-]"  #'xref-find-definitions-other-window)
	  
;; C+c r will give clang bindings... pausing for thought
;; will result in reminders appearing
(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))
