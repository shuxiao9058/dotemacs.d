;;; lisp/init-rime.el -*- lexical-binding: t; -*-

(use-package rime
    :straight (
	       :host github
	       :repo "DogLooksGood/emacs-rime"
	       :branch "lib"
               :files ("*.el" "Makefile" "lib.c"))
    :defer 1
    :init
    :custom
    ;; (rime-librime-root (concat user-emacs-directory "librime/dist"))
    (rime-librime-root "/usr/local")
    ;; (rime-librime-path "/usr/local")
    (rime-show-candidate 'posframe)
    ;; (rime-share-data-dir "")
    (rime-user-dir (expand-file-name "rime" poly-local-dir))
    :config
    (setq default-input-method "rime"
          ;; rime-show-candidate 'overlay
          ;; rime-show-candidate 'message
          ;; rime-show-candidate 'minibuffer
          )
    )

(provide 'init-rime)
;;; init-rime.el ends here
