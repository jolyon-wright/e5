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
(use-package magit)
(use-package vdiff-magit)

(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;; This flag will default to using ediff for merges.
(setq vdiff-magit-use-ediff-for-merges nil)

;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
;; when point is on an uncommitted hunk.
(setq vdiff-magit-dwim-show-on-hunks nil)

;; Whether vdiff-magit-show-stash shows the state of the index.
(setq vdiff-magit-show-stash-with-index t)

;; Only use two buffers (working file and index) for vdiff-magit-stage
(setq vdiff-magit-stage-is-2way nil)


(provide 'jw-vdiff)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jw-vdiff.el ends here.
