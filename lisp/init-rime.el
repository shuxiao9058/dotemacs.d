;;; lisp/init-rime.el -*- lexical-binding: t; -*-

(use-package rime
    :straight (rime
	       :host github
	       :repo "DogLooksGood/emacs-rime"
               :files (:defaults "rime.el" "lib.c" "Makefile"))
    :defer t
    :hook
    ('kill-emacs . (lambda ()
                     (when (fboundp 'rime-sync)
                       (ignore-errors (rime-sync)))))
    :custom
    (default-input-method "rime")
    ;; (rime-librime-root (concat user-emacs-directory "librime/dist"))
    (rime-librime-root "/usr/local")
    (rime-show-candidate 'posframe)
    (rime-posframe-properties (list :background-color "#202325"
				    :foreground-color "#ddddde" ;; "#dedddd"
				    :internal-border-width 6))
    (rime-code-face
     '((t (:inherit default :background "#ffffff" :foreground "#000000"))))
    (rime-disable-predicates
          '(evil-normal-state-p
            rime--after-alphabet-char-p
            rime--prog-in-code-p
            ))
    ;; (rime-share-data-dir "")
    (rime-user-data-dir (expand-file-name "rime" poly-local-dir)))

(provide 'init-rime)
;;; init-rime.el ends here
