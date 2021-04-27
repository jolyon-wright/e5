;; -*- lexical-binding: t; -*-
;; jolyon's emacs configuration

(defvar bootstrap-version)
(setq straight-vc-git-default-clone-depth 1)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (setq straight-recipe-overrides
          '((nil . ((straight :host github
                              :repo "jolyon-wright/straight.el"
                              :branch "master"
                              :files ("straight.el"))))))
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(straight-use-package '(jolyon929 :host bitbucket  :repo "jolyon929/e5" :branch "cmn"))
(load (concat user-emacs-directory (expand-file-name "straight/repos/e5/jw-cmn.el" user-emacs-directory)))

