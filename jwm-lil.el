;;; jwm-lil.el --- Description    -*- lexical-binding: t; -*-

;; Author: Jolyon Wright  jolyon@Jolyons-MacBook-Pro.local
;; Keywords:
;; URL:

;; Copyright (C) 2022, Jolyon Wright, all rights reserved.

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

;; example org:-

;; #+begin_src lilypond :file eg.png
;; \version "2.22.1"

;; \header { }

;; \score {
;;   {a b c}
;;   \layout { }
;;   \midi { }
;; }
;; #+end_src



;;; Code:

(use-package lilypond-init
  :straight nil
  ;; :load-path "/usr/local/Cellar/lilypond/2.22.1_1/share/emacs/site-lisp/lilypond"
  :load-path "/usr/local/share/emacs/site-lisp/lilypond"
  :custom
  (org-babel-lilypond-commands '("lilypond" "open" "open")
                               "Commands to run lilypond and view or play the results.
These should be executables that take a filename as an argument.
On some system it is possible to specify the filename directly
and the viewer or player will be determined from the file type;
you can leave the string empty on this case."
  :group 'org-babel
  :type '(list
	  (string :tag "Lilypond   ")
	  (string :tag "PDF Viewer ")
	  (string :tag "MIDI Player"))
  :version "24.4"
  :package-version '(Org . "8.2.7")
  :set
  (lambda (symbol value)
    (set symbol value)
    (setq
     org-babel-lilypond-ly-command   (nth 0 value)
     org-babel-lilypond-pdf-command  (nth 1 value)
     org-babel-lilypond-midi-command (nth 2 value))))

  :config
  ;;:init
  (message "* jw lilypond-init ; config")
  (setq org-babel-lilypond-arrange-mode t
   ;; hardcoded as     '("/Applications/lilypond.app/Contents/Resources/bin/lilypond" "open" "open"))
  ;;      org-babel-lilypond-commands '("lilypond" "open" "open")
        org-babel-lilypond-gen-pdf nil
        org-babel-lilypond-display-pdf-post-tangle nil)
  :mode ("\\.ly\\'" . LilyPond-mode))



 ;; https://github.com/brailcom/singing-computer

;; brew install speech-tools
;; (straight-use-package '(brailcom :type git :host github :repo "brailcom/singing-computer"))


(provide 'jwm-lil)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-lil.el ends here.
