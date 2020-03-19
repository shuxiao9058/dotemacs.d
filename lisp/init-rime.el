;;; lisp/init-rime.el -*- lexical-binding: t; -*-

(use-package rime
    :straight (rime
	       :host github
	       :repo "DogLooksGood/emacs-rime"
               :files (:defaults "rime.el" "lib.c" "Makefile"))
    :defer t
    :custom
    (default-input-method "rime")
    ;; (rime-librime-root (concat user-emacs-directory "librime/dist"))
    (rime-librime-root "/usr/local")
    (rime-show-candidate 'posframe)
    ;; (rime-share-data-dir "")
    (rime-user-data-dir (expand-file-name "rime" poly-local-dir)))

(provide 'init-rime)
;;; init-rime.el ends here
