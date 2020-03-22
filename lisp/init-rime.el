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
    (rime-user-data-dir (expand-file-name "rime" poly-local-dir))
    :config
    (progn
      (setq-default rime-disable-predicates
		    '(+rime-predicate-button-at-point-p
                      rime-predicate-evil-mode-p
                      rime-predicate-punctuation-line-begin-p
                      rime-predicate-after-alphabet-char-p
                      rime-predicate-prog-in-code-p
                      +rime-predicate-beancount-p))
      (setq-default rime-inline-predicates
		    '(+rime-predicate-current-input-uppercase-letter-p))

      (defun ac-rime-set-text-mode-predicate()
	  ;;; set text-mode
	(setq-local rime-disable-predicates
		    '(+rime-predicate-button-at-point-p
		      rime-predicate-evil-mode-p
		      rime-predicate-punctuation-line-begin-p
		      +rime-predicate-puncutuation-after-space-cc-p
		      +rime-predicate-puncutuation-after-ascii-p))
	(setq-local rime-inline-predicates
		    '(+rime-predicate-current-input-uppercase-letter-p
		      rime-predicate-auto-english-p))
	)

      (eval-after-load 'telega
	(add-hook 'telega-chat-mode-hook #'ac-rime-set-text-mode-predicate)
	)
      (add-hook 'text-mode-hook #'ac-rime-set-text-mode-predicate)
      )
    )

;;;
;;; https://github.com/cnsunyour/.doom.d/blob/develop/modules/cnsunyour/chinese/%2Brime-probe-english.el
;;;
(defun +rime-predicate-current-input-uppercase-letter-p ()
  "If the current charactor entered is a uppercase letter.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (>= rime--current-input-key ?A)
       (<= rime--current-input-key ?Z)))

(defun +rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (looking-back "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]" 1)))

(defun +rime-predicate-current-input-punctuation-p ()
  "If the current charactor entered is a punctuation.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (or (and (<= #x21 rime--current-input-key) (<= rime--current-input-key #x2f))
           (and (<= #x3a rime--current-input-key) (<= rime--current-input-key #x40))
           (and (<= #x5b rime--current-input-key) (<= rime--current-input-key #x60))
           (and (<= #x7b rime--current-input-key) (<= rime--current-input-key #x7f)))))

(defun +rime-predicate-puncutuation-after-space-cc-p ()
  "If input a punctuation after a Chinese charactor with whitespace.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'.\""
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (+rime-predicate-current-input-punctuation-p)
       (looking-back "\\cc +" 2)))

(defun +rime-predicate-puncutuation-after-ascii-p ()
  "If input a punctuation after a ascii charactor with whitespace.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (+rime-predicate-current-input-punctuation-p)
       (+rime-predicate-after-ascii-char-p)))

(defun +rime-predicate-button-at-point-p ()
  "Determines whether the point is a button.
\"Button\" means that positon is not editable.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode'.
Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(provide 'init-rime)
;;; init-rime.el ends here
