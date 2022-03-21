;;;; init.el -*- lexical-binding: t; -*-

;; Author: Jolyon Wright  jolyon@Jolyons-MacBook-Pro.local
;; Keywords: 
;; URL: 

;; Copyright (C) 2021, Jolyon Wright, all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; todo - what is this -- ? https://gitlab.com/buildfunthings/emacs-config
;;

(defvar jw-radian--init-file-loaded-p nil) ;; this is a sort of header guard
(unless jw-radian--init-file-loaded-p      ;; because we *may* be pulled in
                                           ;; by early init

  (setq jw-radian--init-file-loaded-p t)

  ;; partial implementation of shallow depth/ single branch on develop:-
  ;;(setq straight-repository-branch "develop")
  (setq straight-vc-git-default-clone-depth 1)
  (defvar bootstrap-version)

  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      ;; use this repo for the mo:-
      (setq straight-recipe-overrides
            '((nil . ((straight :host github
                                :repo "jolyon-wright/straight.el"
                                :branch "master"
                                :files ("straight.el"))))))
      ;; remove above when develop is finalized
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; (setq straight-vc-git-default-clone-depth '(1 single-branch)) ;; the new idiom!
  ;; remove above when develop is finalized

  (setq straight-use-package-by-default t)
;;  (setq use-package-always-defer t) ;;? seems to break everything!
  (straight-use-package 'use-package)
  (require 'use-package)


  ;; omg
;;; Prevent Emacs-provided Org from being loaded

  ;; Our real configuration for Org comes much later. Doing this now
  ;; means that if any packages that are installed in the meantime
  ;; depend on Org, they will not accidentally cause the Emacs-provided
  ;; (outdated and duplicated) version of Org to be loaded before the
  ;; real one is registered.

  ;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el

  (straight-register-package 'org)
  (straight-register-package 'org-plus-contrib)

  ;; omg ends

  (defvar jw-e5-base (concat user-emacs-directory "straight/repos/e5") "the root of e5")

  ;; don't try to load byte compiled lisp into the wrong emacs (shudder)
  (eval
   `(unless (equal
             (emacs-version), (eval-when-compile (emacs-version)))
      (mapc 'delete-file (append (directory-files-recursively jw-e5-base ".*\\.elc$" nil)
                                 (directory-files user-emacs-directory t ".*\\.elc$")))))

  ;; (use-package shut-up)

  (defun shut-up (arg))
  
  (load (expand-file-name "jw-defaults.el" user-emacs-directory))

  (unless (file-exists-p jw-e5-base)
    (make-directory jw-e5-base))

  (defun jw-get-lisp (&optional branch)
    "get a single branch; nb - it wont refresh!"
    (interactive "sWhich branch to pull?")
    (let* ((default-directory jw-e5-base)
           (branch-dir (concat jw-e5-base "/" branch)))
      (if (file-directory-p branch-dir)
          (progn
            (jw-setup-lisp-dir branch-dir)
            (jw-setup-lisp-pull branch-dir))
        (let* ((git-repo " https://bitbucket.org/jolyon929/e5.git ")
               (git-command (concat "git clone --single-branch -b " branch git-repo branch))
               (output-buffer (generate-new-buffer (format "*clone %s*" branch)))
               (proc (progn
                       (async-shell-command git-command output-buffer)
                       (get-buffer-process output-buffer))))
          (if (process-live-p proc)
              (progn
                (process-put proc 'jw-branch-dir branch-dir)
                (set-process-sentinel proc #'(lambda (process signal)
                                               (when (memq (process-status process) '(exit signal))
                                                 (jw-setup-lisp-remote (process-get process 'jw-branch-dir))
                                                 (jw-setup-lisp-dir (process-get process 'jw-branch-dir))
                                                 (shell-command-sentinel process signal)))))
            (message "ouch; No process running."))))))

  ;; minimal set of stuff:-
  (mapc 'jw-get-lisp '("col" ;; color theme
                       ;; "cmp" ;; company mode
                       ;; "dev" ;; cmode etc
                       ;; "org" ;; big !
                       ;; "flf" ;; overflow - big !
		               ;; "dsk" ;; desktop
                       ;; "rtg" ;; rtags
                       ;; "lsp"
                       ;; "py"
                       ;; "jen"
                       ;; "mov"
                       ;; "clj"
                       ;; "scl" ;; common lisp
                       ;; "vtm" ;; needs strangeness
                       ;; "chi" ;; mandarin
                       ))

  (set-face-attribute 'default nil :height 160)

  ;; hint for local.el:-
  ;; (mapc 'jw-get-lisp '(
  ;;                      "cmp" ;; company mode
  ;;                      "dev" ;; cmode etc
  ;;                      ;; "org" ;; big !
  ;;                      ;; "flf" ;; overflow - big !
  ;;   	               "dsk" ;; desktop
  ;;                      ;; "rtg" ;; rtags
  ;;                      ;; "lsp"
  ;;                      ;; "py"
  ;;                      ;; "jen"
  ;;                      ;; "mov"
  ;;                      ;; "clj"
  ;;                      ;; "scl" ;; common lisp
  ;;                      ;; "vtm" ;; needs strangeness
  ;;                      ;; "chi" ;; mandarin
  ;;                      ))


  (let ((lcl (expand-file-name "local.el" user-emacs-directory)))
    (when (file-exists-p lcl)
      (load lcl))))

;; needs to be last:-
(when (equal window-system 'w32)
  (setq
   w32-pass-lwindow-to-system nil
   w32-lwindow-modifier 'super
   w32-pass-rwindow-to-system nil
   w32-rwindow-modifier 'super
   w32-pass-apps-to-system nil
   w32-apps-modifier 'hyper
   w32-pass-alt-to-system nil
   w32-scroll-lock-modifier nil))
