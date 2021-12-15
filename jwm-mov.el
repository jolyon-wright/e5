;;; jwm-mov.el --- Description    -*- lexical-binding: t; -*-

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


(use-package pvr)
(use-package imdb)
(use-package touchgrid)
(use-package movie)

(defvar movie-player
  '("/usr/local/bin/mpv"
    "--audio-device=auto"
    "--vo=gpu"
    "--hwdec=vdpau"
    ;;"--vf=vdpaupp=denoise=1"
    ;;"--tone-mapping=clip" "--tone-mapping-param=1"
    "--input-ipc-server=/tmp/mpv-socket"
    "--fullscreen"
    "--stop-screensaver"
    )
  "Command to play a file.")

(straight-use-package '(skeeto :type git :host github :repo "skeeto/youtube-dl-emacs"))

;; org to embed a video that can be played with wxwidgets:-

;; #+ATTR_HTML: :controls controls :width 350
;; #+BEGIN_video
;; #+HTML: <source src="/Users/jolyon/jack.mp4" type="video/mp4">
;; Your browser does not support the video tag.
;; #+END_video

;; emacs 28 build settings:-
;; system-configuration-options is a variable defined in `C source code'.

;; Its value is
;; "--with-native-compilation --with-cairo --with-xwidgets
;; CPPFLAGS=-I/usr/local/opt/llvm/include
;; LDFLAGS=-L/usr/local/opt/llvm/lib"



(provide 'jwm-mov)

;; Local Variables:
;; coding: utf-8
;; End:

;;; jwm-mov.el ends here.
