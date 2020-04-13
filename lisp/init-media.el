;;; lisp/init-media.el -*- lexical-binding: t; -*-

(use-package emms
  :straight t
  :commands (emms)
  :custom
  (mpc-host "localhost:6610")
  (emms-seek-seconds 5)
  (emms-player-list '(emms-player-mpd))
  (emms-info-functions '(emms-info-mpd))
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6610")

  :config
  (progn
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all) ))

(use-package bongo
  :straight t
  :commands bongo-playlist
  :custom (bongo-enabled-backends '(mplayer)))

(use-package simple-mpc
  :straight t
  :commands (simple-mpc)
  :custom
  (simple-mpc-playlist-format "%artist%	%album%	%title%	%file%")
  (simple-mpc-table-separator " "))

(use-package volume
  :straight t
  :after bongo)

(use-package netease-music
  :straight t
  ;; :defer t
  :init
  (setq netease-music-username "shuxiao9058")
  (setq netease-music-user-password "7Z2GcG6y)])849x")
  (setq netease-music-user-id "58665026")
  (setq netease-music-api "http://localhost:3000"))

(provide 'init-media)
;;; init-media.el ends here
