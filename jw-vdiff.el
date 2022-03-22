;;; jw-vdiff.el --- Description    -*- lexical-binding: t; -*-

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

;;; Commentary:

;;  https://github.com/justbur/emacs-vdiff-magit

;;; Code:
;;(require 'vdiff-magit)
;; (use-package magit)
;; (use-package vdiff-magit)

;; (define-key magit-mode-map "e" 'vdiff-magit-dwim)
;; (define-key magit-mode-map "E" 'vdiff-magit)
;; (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
;; (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
;; (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
;; (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;; ;; This flag will default to using ediff for merges.
;; (setq vdiff-magit-use-ediff-for-merges nil)

;; ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
;; ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
;; ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
;; ;; when point is on an uncommitted hunk.
;; (setq vdiff-magit-dwim-show-on-hunks nil)

;; ;; Whether vdiff-magit-show-stash shows the state of the index.
;; (setq vdiff-magit-show-stash-with-index t)

;; ;; Only use two buffers (working file and index) for vdiff-magit-stage
;; (setq vdiff-magit-stage-is-2way nil)

;; https://gitlab.com/buildfunthings/emacs-config/-/blob/master/loader.org

;; im not sure this works!


(use-package hideshow
  :bind (("C-c C-h >" . my-toggle-hideshow-all)
         ("C-c C-h <" . hs-hide-level)
         ("C-;" . hs-toggle-hiding)
         ("M-<f1>" . hs-toggle-hiding)
         )
  :config
  ;; Hide the comments too when you do a 'hs-hide-all'
  (setq hs-hide-comments nil)
  ;; Set whether isearch opens folded comments, code, or both
  ;; where x is code, comments, t (both), or nil (neither)
  (setq hs-isearch-open t)
  ;; Add more here

  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format " ... <%d> "
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'font-lock-type-face)))))

  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
       ;;;###autoload
  (defun my-toggle-hideshow-all () "Toggle hideshow all."
         (interactive)
         (setq my-hs-hide (not my-hs-hide))
         (if my-hs-hide
             (hs-hide-all)
           (hs-show-all)))

  (add-hook 'prog-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              ))
  (add-hook 'lisp-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              ))
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              )))


(use-package magit
  ;; :bind (("C-c m" . magit-status))
  :config (setq transient-default-level 5))

(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package forge)

(use-package git-timemachine)

;; https://github.com/alphapapa/unpackaged.el#smerge-mode
;; Tipped by Mike Z.
(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

Display the buffer state in the fringe.
(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " "))


(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(provide 'jw-vdiff)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jw-vdiff.el ends here.
