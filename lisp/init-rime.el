;;; lisp/init-rime.el -*- lexical-binding: t; -*-

(defun +rime-predicate-is-back-quote-or-tilde ()
  (or (equal rime--current-input-key ?`)
      (equal rime--current-input-key ?~)))

(use-package rime
    :straight (rime
	       :host github
	       :repo "DogLooksGood/emacs-rime"
               :files (:defaults "lib.c" "Makefile"))
    :defer t
    :custom
    (rime-disable-predicates '(rime-predicate-prog-in-code-p
                               rime-predicate-after-alphabet-char-p
			       meow-normal-mode-p
                               meow-motion-mode-p
                               meow-keypad-mode-p))
    (rime-inline-predicates '(rime-predicate-space-after-cc-p
                              +rime-predicate-is-back-quote-or-tilde
                              rime-predicate-current-uppercase-letter-p))
    (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
    (default-input-method "rime")
    ;; (rime-cursor "˰")
    ;; (rime-librime-root (concat user-emacs-directory "librime/dist"))
    (rime-librime-root "/opt/local")
    ;; (rime-show-candidate 'minibuffer)
    (rime-show-preedit t)
    (rime-show-candidate 'posframe)
    ;; (rime-show-candidate 'minibuffer)
    ;; (rime-posframe-properties (list :background-color "#202325"
    ;; 				  :foreground-color "#ddddde" ;; "#dedddd"
    ;; 				  :internal-border-width 6))
    ;; (rime-code-face
    ;;  '((t (:inherit default :background "#ffffff" :foreground "#000000"))))
    ;; (rime-disable-predicates
    ;;  '(evil-normal-state-p
    ;;    rime--after-alphabet-char-p
    ;;    rime--prog-in-code-p
    ;;    ))
    ;; (rime-share-data-dir "")
    (rime-user-data-dir (expand-file-name "rime" poly-local-dir))
    :bind
    (:map rime-active-mode-map
	  ("<tab>" . rime-inline-ascii)
	  :map rime-mode-map
	  ("C-$" . rime-send-keybinding)
	  ("M-j" . rime-force-enable))
    ;; :config
    )

(provide 'init-rime)
;;; init-rime.el ends here
