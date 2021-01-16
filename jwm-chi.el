(require 'josh-chinese)


(when (display-graphic-p)

  ;; (font-family-list)
  ;;
  ;; sudo cp ./SourceHanSerifSC-Regular.otf /usr/local/share/fonts/
  ;; sudo fc-cache -fv

  ;; if needed:-
  ;; git clone https://github.com/xeyownt/chess_merida_unicode.git


  ;; sudo cp chess_merida_unicode.ttf /usr/local/share/fonts/
  ;; sudo fc-cache -fv

  ;; # sudo apt-get install language-pack-zh*
  ;;   # sudo apt-get install chinese*


  (let ((chess-font "Chess Merida Unicode"))
    (set-fontset-font "fontset-startup"  '(#x1FA00 . #x1FA6F) chess-font)
    (set-fontset-font "fontset-default"  '(#x1FA00 . #x1FA6F) chess-font)
    (set-fontset-font "fontset-standard" '(#x1FA00 . #x1FA6F) chess-font)
    )

  (let ((cjk-font "Source Han Serif SC"))

    (set-fontset-font "fontset-startup"  '(#x000000 . #x3FFFFF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x000000 . #x3FFFFF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x000000 . #x3FFFFF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x3300 .  #x33FF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x3300 .  #x33FF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x3300 .  #x33FF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#xFE30 .  #xFE4F) cjk-font)
    (set-fontset-font "fontset-default"  '(#xFE30 .  #xFE4F) cjk-font)
    (set-fontset-font "fontset-standard" '(#xFE30 .  #xFE4F) cjk-font)

    (set-fontset-font "fontset-startup"  '(#xF900  . #xFAFF) cjk-font)
    (set-fontset-font "fontset-default"  '(#xF900  . #xFAFF) cjk-font)
    (set-fontset-font "fontset-standard" '(#xF900  . #xFAFF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x2F800 . #x2FA1F) cjk-font)
    (set-fontset-font "fontset-default"  '(#x2F800 . #x2FA1F) cjk-font)
    (set-fontset-font "fontset-standard" '(#x2F800 . #x2FA1F) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x2E80  . #x2EFF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x2E80  . #x2EFF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x2E80  . #x2EFF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x31C0  . #x31EF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x31C0  . #x31EF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x31C0  . #x31EF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x3000  . #x303F) cjk-font)
    (set-fontset-font "fontset-default"  '(#x3000  . #x303F) cjk-font)
    (set-fontset-font "fontset-standard" '(#x3000  . #x303F) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x4E00  . #x9FFF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x4E00  . #x9FFF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x4E00  . #x9FFF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x3400  . #x4DBF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x3400  . #x4DBF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x3400  . #x4DBF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x20000 . #x2A6DF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x20000 . #x2A6DF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x20000 . #x2A6DF) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x2A700 . #x2B73F) cjk-font)
    (set-fontset-font "fontset-default"  '(#x2A700 . #x2B73F) cjk-font)
    (set-fontset-font "fontset-standard" '(#x2A700 . #x2B73F) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x2B740 . #x2B81F) cjk-font)
    (set-fontset-font "fontset-default"  '(#x2B740 . #x2B81F) cjk-font)
    (set-fontset-font "fontset-standard" '(#x2B740 . #x2B81F) cjk-font)

    (set-fontset-font "fontset-startup"  '(#x2B820 . #x2CEAF) cjk-font)
    (set-fontset-font "fontset-default"  '(#x2B820 . #x2CEAF) cjk-font)
    (set-fontset-font "fontset-standard" '(#x2B820 . #x2CEAF) cjk-font)
    )


  (dolist (item '(
                  ("Source Han Serif SC" . 1.25)
                  ("Chess Merida Unicode" . 2.0)
                  ))
    (add-to-list 'face-font-rescale-alist item))
  ;; (set-face-attribute 'default nil :height 180)
  ;; after inclusion in .emacs for sensible height
  )


(defun jw-get-cangjie ()
  (interactive)
  (message "%s" (josh/chinese-cangjie-codes(thing-at-point 'word)))
  )

(defun jw-get-english ()
  (interactive)
  (message "%s" (josh/chinese-dict-find(thing-at-point 'word)))
  )


(straight-use-package 'google-translate)

(defun jw-cn2en ()
  (setq google-translate-default-source-language "zh-CN")
  (setq google-translate-default-target-language "en")
  (google-translate-at-point)
  )

(defun jw-en2cn ()
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "zh-CN")
  (google-translate-at-point)
  )

(defun jw-translate ()
  (interactive)
  (if (looking-at "[a-z-A-Z]")
      (jw-en2cn)
    ;; a weakness - i assume non ascii must be chinese
    (jw-cn2en)))

(bind-key "s-c" 'jw-translate)
;; for example - cursor on dog, hit super+c you will get 狗
;;               cursor on 狗, hit super+c you will get "dog"
;;
;; dog
;; 狗
(register-input-method
 "Cangjie5" "Chinese-BIG5" 'quail-use-package
 "C5" "Cangjie version 5"
 "cangjie5.el")

(defun jw-set-Cangjie5() (interactive) (set-input-method "Cangjie5"))
(defun jw-set-chinese-sisheng() (interactive) (set-input-method "chinese-sisheng"))
(defun jw-set-chinese-tonepy() (interactive) (set-input-method "chinese-tonepy"))
(defun jw-set-chinese-py() (interactive) (set-input-method "chinese-py"))
(defun jw-set-piyim() (interactive) (set-input-method "pyim"))

(bind-key  "C-#"       'josh/chinese-def-at-point)
(bind-key  "C-="       'josh/chinese-decomposition-at-point)
(bind-key  "C-M-="     'jw-get-english)

(use-package pyim
  :defer
  )

(bind-key "<s-f7>" 'toggle-input-method)
(bind-key "<s-f2>" 'jw-set-chinese-sisheng)
(bind-key "<s-f3>" 'jw-set-chinese-tonepy)
(bind-key "<s-f4>" 'jw-set-chinese-py)
(bind-key "<s-f5>" 'jw-set-Cangjie5)
(bind-key "<s-f6>" 'jw-set-piyim)
(bind-key "<s-f10>" 'jw-get-cangjie)
(bind-key "<C-f10>" 'jw-get-english)
