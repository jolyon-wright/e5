
(if (eq system-type 'windows-nt)
    (message "vterm is broken on wondows - revisit"))

(when (not (equal window-system 'w32))
  (use-package vterm
    :defer t
    :init
    (setq vterm-always-compile-module t)
    )
  ;; on mac:-
  ;; brew install libvterm
  (use-package multi-vterm )
  (bind-key "<C-f9>" 'multi-vterm))


