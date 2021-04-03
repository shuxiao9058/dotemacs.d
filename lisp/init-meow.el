;; lisp/init-meow.el -*- lexical-binding: t; -*-

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   )
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("x" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-join)
   '("M" . delete-indentation)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("z" . meow-pop)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-search)
   '("<escape>" . meow-last-buffer)
   '("?" . meow-cheatsheet)
   ))

(use-package meow
  :straight t
  :demand t
  :ensure t
  :init
  (meow-global-mode 1)
  :custom
  (meow-expand-hint-remove-delay 3.0)
  :config
  (add-to-list 'meow-char-thing-table '(?\] . line))
  (add-to-list 'meow-char-thing-table '(?\[ . line))

  (dolist (hook '(git-commit-mode-hook
		  magit-log-edit-mode-hook))
    (add-hook hook (lambda()(meow--switch-state 'insert))))

  ;; disable <backspace> work as meow-keypad-undo
  ;; since some useful command may use <backspace> key
  ;; such as C-x BS
  (define-key meow-keypad-state-keymap (kbd "<backspace>")
    'meow-keypad-self-insert)

  (meow-leader-define-key
   '("e" . my/icomplete-recentf)
   '("." . find-file)
   '("p" . projectile-command-map)
   '("b" . switch-to-buffer)
   '("s" . save-buffer)
   )
  ;; meow-setup is your custom function, see below
  (meow-setup)
  ;; If you want relative line number in NORMAL s tate(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing by hand.
  (meow-setup-indicator))

(provide 'init-meow)

;;; init-meow.el ends here