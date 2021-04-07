
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

  (defun jw-new-vshell ()
    (interactive)
    (let (
          (currentbuf (get-buffer-window (current-buffer)))
          (newbuf     (generate-new-buffer-name "*vterm*"))
          )
      (generate-new-buffer newbuf)
      (set-window-dedicated-p currentbuf nil)
      (set-window-buffer currentbuf newbuf)
      (vterm newbuf)
      )
    )

  ;; (bind-key "<C-f9>" #'vterm)
  (bind-key "<C-f9>" 'jw-new-vshell)
  )
