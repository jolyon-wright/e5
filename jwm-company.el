;;; jwm-company.el --- Description    -*- lexical-binding: t; -*-

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

;;

;;; Code:
;;

;; use ctrl+enter to select.
(straight-use-package '(jolyon929 :type git :host bitbucket :repo "jolyon929/company-mode"))

(bind-key "<C-f3>" #'global-company-mode)
(setq company-global-modes '(not text-mode org-mode))
(setq company-show-quick-access t)

(global-company-mode)


(use-package company-prescient
  :init (company-prescient-mode 1))

;; https://oremacs.com/2017/12/27/company-numbers/
(provide 'jwm-company)


;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-company.el ends here.
