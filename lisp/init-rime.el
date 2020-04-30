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
  ;; :hook
  ;; ('kill-emacs . (lambda ()
  ;;                  (when (fboundp 'rime-sync)
  ;;                    (ignore-errors (rime-sync)))))
  :custom
  (rime-disable-predicates '(rime-predicate-prog-in-code-p
                             rime-predicate-after-alphabet-char-p))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p
                            +rime-predicate-is-back-quote-or-tilde
                            rime-predicate-current-uppercase-letter-p))
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (default-input-method "rime")
  ;; (rime-cursor "˰")
  ;; (rime-librime-root (concat user-emacs-directory "librime/dist"))
  (rime-librime-root "/usr/local")
  (rime-show-candidate 'minibuffer)
  ;; (rime-show-candidate 'posframe)
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
  :general
  ("M-SPC" #'toggle-input-method)
  (:keymaps '(rime-active-mode-map)
	    "<tab>"  'rime-inline-ascii)
  (:keymaps '(rime-mode-map)
	    "C-$" #'rime-send-keybinding
	    "M-j"  #'rime-force-enable)
  )

;; (use-package smart-input-source
;;   :straight t
;;   :hook ((text-mode prog-mode) . smart-input-source-mode)
;;   :config
;;   (setq smart-input-source-english-input-source
;;         "com.apple.keylayout.US")
;;   (setq smart-input-source-other-input-source
;;         "im.rime.inputmethod.Squirrel.Rime")
;; )

(provide 'init-rime)
;;; init-rime.el ends here
