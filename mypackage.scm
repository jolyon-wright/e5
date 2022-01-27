;; install guix:-
;;   wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
;;   chmod +x guix-install.sh
;;   sudo ./guix-install.sh
;;
;; get the specific snapshots from here:-
;; this doesnt work
;;   guix --pull 3d6040c
;;
;; grab these packages:-
;;   guix package --manifest=mypackage.scm

(specifications->manifest '("emacs"
                            "guile"
                            "emacs-geiser"
                            "tree"
                            "cmake"
                            "font-liberation"
                            ;; "sbcl"
                            "rtags"
                            "baobab"
                            "kitty"
                            ;; boost doesnt seem to work
                            ;; "boost-static"
                            "llvm"

                            ;; not tried:-
                            "glibc-utf8-locales"
                            "clang"
                            "bear"
                            ;; "emacs-guix"
                            ))
