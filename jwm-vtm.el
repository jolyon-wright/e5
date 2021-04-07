
(if (eq system-type 'windows-nt)
    (message "vterm is broken on wondows - revisit")
                                        ;(straight-use-package 'vterm)
  (use-package vterm
    :defer t
    :init
    (setq vterm-always-compile-module t)
    )
  )
;; on mac:-
; brew install libvterm

(when (not (equal window-system 'w32))
  (bind-key "<C-f9>" #'vterm)
  )
