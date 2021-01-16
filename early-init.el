;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

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

(setq package-enable-at-startup nil)
(setq native-comp-speed 2
      comp-speed 2)
(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t
      comp-async-query-on-exit t)

(setq comp-deferred-compilation t)

(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)"
                   ;; Don't native-compile *-authloads.el and *-pkg.el files as they
                   ;; seem to produce errors during native-compile.
                   "\\(?:[^z-a]*-autoloads\\.el$\\)"
                   "\\(?:[^z-a]*-pkg\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

(setq frame-inhibit-implied-resize t)
(setq gc-cons-threshold most-positive-fixnum)

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;;; early-init.el ends here
