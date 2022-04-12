;;; jolyon-theme.el --- Jolyon-based custom theme for faces

;; Copyright (C)

;; Authors:
;;     jolyon

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; The colors in this theme come from the Jolyon palette, which is in
;; the public domain:

;; reminder - m-x list-colors-display

;;; Code:

(deftheme jolyon
  "Face colors using the Jolyon palette (dark background).
Basic, Font Lock, Isearch, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Jolyon palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; Not in Jolyon palette; used for better contrast.
      (cham-4 "#346604") (blue-0 "#8cc4ff") (orange-4 "#b35000")
      ;; see http://raebear.net/comp/emacscolors.html
      (black "#000000") (cyan "#00ffff") (yellow "#cdcd00")
      (white "#ffffff") (blue "#0000ff") (green "#00cd66")
      (skyBlue "#00bfff") (lightGreen "#90ee90") (paleTurqoise "#bbffff")
      (steelBlue "#cae1ff") (indianRed "#ff6a6a") (goldenRod "#ffc125")
      (honeydew "#f0fff0") (aquamarine "#7fffd4") (midnightblue "#191970")
      (mediumAquamarine "#66cdaa") (darkSlateGray  "#2f4f4f")
      (slateGray "#708090") (lightSlateGray "#778899")
      (darkSeaGreen "#698b69")
)

  (custom-theme-set-faces
   'jolyon
   `(default ((,class (:foreground ,white :background ,black))))
   `(cursor ((,class ( :background ,steelBlue))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,black))))

   `(highlight ((,class ( :background ,midnightblue))))

   `(region ((,class (:background ,red-3))))

   `(secondary-selection ((,class (:background ,blue-0))))
  ;; `(isearch ((,class (:foreground "#ffffff" :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,choc-1))))
   `(trailing-whitespace ((,class (:background ,red-1))))
   ;; Mode line faces
   `(mode-line ((t (
                    :background ,"darkSeaGreen"
                                :foreground ,"black"
                                :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
                                       :background ,"darkSlateGray"
                                       :foreground ,"darkSeaGreen"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,skyBlue))))
   `(escape-glyph ((,class (:foreground ,red-3))))
   `(error ((,class (:foreground ,red-3))))
   `(warning ((,class (:foreground ,orange-3))))
   `(success ((,class (:foreground ,cham-3))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground , paleTurqoise))))
   `(font-lock-constant-face ((,class (:weight bold :foreground ,"#ffe7ba"))))
   ;;`(font-lock-function-name-face ((,class (:foreground , indianRed))))
   `(font-lock-function-name-face ((,class (:foreground , "plum"))))

   `(font-lock-keyword-face ((,class (:foreground ,goldenRod))))
   `(font-lock-string-face ((,class (:foreground ,yellow))))
   `(font-lock-type-face ((,class (:foreground ,steelBlue))))

   ;;`(font-lock-variable-name-face ((,class (:foreground ,orange-4))))
   `(font-lock-variable-name-face ((,class (:foreground ,"orange"))))

   ;; Button and link faces
   ;;`(link ((,class (:underline t :foreground ,blue-3))))
   `(link-visited ((,class (:underline t :foreground ,"LightSkyBlue1"))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-3))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,red-3))))
   `(message-header-to ((,class (:weight bold :foreground ,butter-3))))
   `(message-cited-text ((,class (:slant italic :foreground ,alum-5))))
   `(message-separator ((,class (:weight bold :foreground ,cham-3))))
   ;; SMerge
   `(smerge-refined-change ((,class (:background ,plum-1))))
   ;; Flyspell
   `(flyspell-duplicate ((,class (:underline ,orange-1))))
   `(flyspell-incorrect ((,class (:underline ,red-1))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline  ,cham-4))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,alum-2))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,alum-2))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:underline  ,orange-3))))
   `(semantic-tag-boundary-face ((,class (:overline   ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline  ,red-1))))

  ;;;;
   `(info-xref ((,class (:foreground ,"Yellow"))) )
   `(button ((,class (:foreground ,"LightSkyBlue1"))) )
   `(rtags-skippedline ((,class (:foreground, "gray54"))) )
   `(rtags-errline ((,class (:weight bold :foreground, "red"))) )
   `(rtags-warnline ((,class (:weight bold :foreground, "orange"))) )
   ;; `(font-lock-comment-face ((t (:foreground, "DeepSkyBlue1" :slant normal)))
   `(font-lock-comment-face ((t (:foreground, "grey50" :slant normal)))
                            )
  ;;;;
   )

  (custom-theme-set-variables
   'jolyon
   `(ansi-color-names-vector [,alum-6 ,red-3 ,cham-3 ,butter-3
                                      ,blue-3 ,plum-3 ,blue-1 ,alum-1])))
(provide-theme 'jolyon)


(custom-set-faces
 '(popup-summary-face ((t (:background "blue" :foreground "grey"))))
 '(popup-tip-face ((t (:background "blue" :foreground "yellow"))))
 '(popup-face ((t (:background "blue" :foreground "yellow"))))

 '(popup-menu-mouse-face ((t (:background "red" :foreground "white"))))
 '(popup-menu-summary-face ((t (:background "red" :foreground "grey"))))
 )

(custom-set-faces

 '(sp-pair-overlay-face ((t (:inherit highlight :background "black" :foreground "yellow")))))
'(sp-wrap-overlay-face ((t (:inherit highlight :background "black" :foreground "yellow"))))
'(sp-wrap-tag-overlay-face((t (:inherit highlight :background "black" :foreground "yellow"))))

