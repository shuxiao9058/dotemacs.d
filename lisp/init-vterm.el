;;; lisp/init-vterm.el -*- lexical-binding: t; -*-

(use-package vterm ; https://github.com/akermu/emacs-libvterm
  :straight t
  :commands (vterm vterm-other-window)
  :general
  (general-nmap vterm-mode-map
    [escape] 'vterm--self-insert
    [return] 'vterm--self-insert
    "p" 'vterm-yank
    "u" 'vterm-undo)
  (general-imap vterm-mode-map
    "C-y" 'vterm-yank)
  (general-def vterm-mode-map
    "M-n" 'vterm-send-down
    "M-p" 'vterm-send-up
    "M-y" 'vterm-yank-pop
    "M-/" 'vterm-send-tab)
  ;; :load-path "~/code/github/emacs-libvterm"
  )

(provide 'init-vterm)
;;; init-vterm.el ends here
