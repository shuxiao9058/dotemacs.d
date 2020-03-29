;;; core/core-evil.el -*- lexical-binding: t; -*-


;; Fix cursor for Evil mode
(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(use-package evil
    :straight t
    :ensure t
    ;; :after evil-leader
    :commands evil-normalize-keymaps
    :init
    (setq ;; evil-want-keybinding nil ;; needed by evil-collection
     evil-want-C-u-scroll t
     evil-search-module 'evil-search)
    :config
    :general
    (nmap "q" nil ;; q quit, not evil-record-macro
	  "Q" #'evil-record-macro)
    :config
    (evil-mode +1)
    (add-to-list 'evil-insert-state-modes 'shell-mode)
    (add-to-list 'evil-insert-state-modes 'dashboard-mode)
    (add-to-list 'evil-insert-state-modes 'git-timemachine-mode)
    (add-to-list 'evil-insert-state-modes 'magit-commit-mode)

    ;; Change cursor color depending on mode
    (setq evil-emacs-state-cursor `("red" hbar))     ; _
    (setq evil-normal-state-cursor `("green" box))   ; █
    (setq evil-visual-state-cursor `("orange" box))  ; █
    (setq evil-insert-state-cursor `("red" bar))     ; ⎸
    (setq evil-replace-state-cursor `("red" bar))
    (setq evil-operator-state-cursor `("red" hollow))
    (setq evil-motion-state-cursor `("orange" box))  ; █

    (add-hook 'after-make-frame-functions
	      (lambda (frame) (my-evil-terminal-cursor-change)))
    (my-evil-terminal-cursor-change)

    ;; set bar cursor for evil-emacs mode
    (setq evil-emacs-state-cursor '(bar))

    (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
      (recenter))

    (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
      (recenter))
    )


;; (use-package evil-leader
;;     :straight t
;;     :init
;;     (setq evil-leader/in-all-states t)
;;     (global-evil-leader-mode)
;;     ;; :config
;;     ;; (evil-leader/set-leader "<SPC>")
;;     )

(use-package evil-escape
    :after evil
    :straight t
    :init
    (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
          evil-escape-key-sequence "jk"
          evil-escape-delay 0.25)
    :config
    (push #'minibufferp evil-escape-inhibit-functions)
    ;; (map! :irvo "C-g" #'evil-escape)
    (evil-escape-mode))

(use-package evil-escape
    :after evil
    :straight t
    :init
    (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
          evil-escape-key-sequence "jk"
          evil-escape-delay 0.25)
    :config
    (push #'minibufferp evil-escape-inhibit-functions)
    ;; (map! :irvo "C-g" #'evil-escape)
    (evil-escape-mode))

(use-package evil-exchange
    :commands evil-exchange
    :straight t)

;;; Press “%” to jump between matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair of tags.
(use-package evil-matchit
    :ensure t
    :commands evilmi-jump-items
    :custom
    (global-evil-matchit-mode t)
    :config
    ;; disable evil-matchit
    (dolist (mode '(magit-status-mode-hook))
      (add-hook mode (lambda()
		       (evil-matchit-mode 0))))
    :general
    ([remap evil-jump-item] #'evilmi-jump-items)
    (nvmap :keymaps '(evil-matchit-mode-map)
	   "%" #'evilmi-text-object)
    )

(use-package evil-snipe
    :commands (evil-snipe-mode evil-snipe-override-mode
			       evil-snipe-local-mode evil-snipe-override-local-mode)
    :straight t
    :init
    (add-hook 'prog-mode-hook 'evil-snipe-mode)
    (setq evil-snipe-smart-case t
          evil-snipe-scope 'line
          evil-snipe-repeat-scope 'visible
          evil-snipe-char-fold t
          evil-snipe-disabled-modes '(magit-mode elfeed-show-mode elfeed-search-mode)
          evil-snipe-aliases '((?\[ "[[{(]")
                               (?\] "[]})]")
                               (?\; "[;:]")))
    :config
    (evil-snipe-mode +1)
    (evil-snipe-override-mode +1))

(use-package evil-surround
    :commands (global-evil-surround-mode
               evil-surround-edit
               evil-Surround-edit
               evil-surround-region)
    :straight t
    :config
    (global-evil-surround-mode 1))

(use-package evil-args
    :commands (evil-inner-arg evil-outer-arg
			      evil-forward-arg evil-backward-arg
			      evil-jump-out-args)
    :straight t)

(use-package evil-commentary
    :commands (evil-commentary evil-commentary-yank evil-commentary-line)
    :straight t
    :after evil
    :config
    (evil-commentary-mode 1)
    :general
    (nvmap "gc" #'evil-commentary
	   "gC" #'evil-commentary-line)
    )

(use-package evil-search-highlight-persist
    :after evil
    :straight t
    :config
    (global-evil-search-highlight-persist t))

(use-package evil-multiedit
    :after evil
    :straight t
    :config
    (evil-multiedit-default-keybinds))

;;; TODO
(use-package evil-mc
    :after evil
    :straight t
    :init
    ;; The included keybindings are too imposing and are likely to cause
    ;; conflicts, so we'll set them ourselves.
    (defvar evil-mc-key-map (make-sparse-keymap))

    :config
    (global-evil-mc-mode  +1)

    ;; REVIEW This is tremendously slow on macos and windows for some reason.
    (setq evil-mc-enable-bar-cursor (not (or IS-MAC IS-WINDOWS)))
    )

(use-package evil-osx-clipboard
    :straight t
    :after pbcopy
    :straight (evil-osx-clipboard
	       :host github
	       :repo "stroxler/evil-osx-clipboard.el"
               :files (:defaults))
    :config
    (evil-osx-clipboard/set-osx-defaults)
    )

;; ;; Evil, probably the best thing since Evil.
;; (use-package evil-collection
;;   :straight t
;;   :after evil
;;   :ensure t
;;   :demand
;;   :init
;;   :config
;;   ;; enabled for all modes provided by default by evil-collection
;;   ;; change this if wanted to enable for specific modes
;;   (evil-collection-init)
;;   :custom
;;   (evil-collection-setup-minibuffer t)
;; )

(provide 'core-evil)
;;; core-evil.el ends here
