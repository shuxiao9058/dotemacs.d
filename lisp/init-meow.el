;; lisp/init-meow.el -*- lexical-binding: t; -*-

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
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
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
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
   '("d" . meow-C-d)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   '("<escape>" . meow-last-buffer)))

(use-package meow
  :straight t
  :demand t
  :ensure t
  :init
  (meow-global-mode 1)
  :custom
  (meow-expand-hint-remove-delay 3.0)
  :config
  (dolist (mode '(inf-iex-mode
		  authinfo-mode
		  Custom-mode
		  cider-test-report-mode
		  comint-mode
		  protobuf-mode
		  cperl-mode
		  perl-mode
		  debuffer-mode
                  ielm-mode
                  inferior-python-mode
                  go-dot-mod-mode
		  go-mod-mode
                  diff-mode))
    (add-to-list 'meow-mode-state-list `(,mode . normal)))

  (add-to-list 'meow-mode-state-list '(git-commit-mode . motion))
  (add-to-list 'meow-mode-state-list '(magit-log-edit-mode . motion))
  (add-to-list 'meow-grab-fill-commands 'eval-expression)

  (setq meow-cursor-type-keypad 'box)
  (setq meow-cursor-type-insert '(bar . 2))

  (setq
   ;; meow-visit-sanitize-completion nil
   meow-esc-delay 0.001
   meow-keypad-describe-delay 0.5
   meow-select-on-change t
   meow-cursor-type-normal 'box
   meow-cursor-type-insert '(bar . 4)
   meow-selection-command-fallback '((meow-replace . meow-page-up)
				     (meow-change . meow-change-char)
				     (meow-save . meow-save-empty)
				     (meow-kill . meow-C-k)
				     (meow-cancel . keyboard-quit)
				     (meow-pop . meow-pop-grab)
				     (meow-delete . meow-C-d)))

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

  (define-key meow-keypad-state-keymap (kbd "DEL")
    'meow-keypad-self-insert)

  (meow-leader-define-key
   '(;; "e" . my/selectrum-recentf-open-files)
     "e" . consult-recent-file)
   ;; '("e" . my/icomplete-recentf)
   ;; '("." . find-file)
   '("." . poly/find-file)
   '("p" . projectile-command-map)
   ;; '("b" . switch-to-buffer)
   '("b" . poly/switch-to-buffer)
   '("s" . save-buffer)
   '("j" . sp-join-sexp)
   '("(" . sp-wrap-round)
   '("[" . sp-wrap-square)
   '("{" . sp-wrap-curly)
   '("o" . ace-window)
   '("a" . delete-other-windows)
   '("-" . split-window-below)
   '("/" . swiper)
   '("\\" . split-window-right)
   '("w" . ace-swap-window)
   '("k" . kill-buffer)
   )
  ;; meow-setup is your custom function, see below
  (meow-setup)
  ;; If you want relative line number in NORMAL s tate(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing by hand.
  (meow-setup-indicator)

  (unbind-key (kbd "<escape>") meow-leader-keymap) ;; disable meow-temp-normal
  )

(provide 'init-meow)

;;; init-meow.el ends here
