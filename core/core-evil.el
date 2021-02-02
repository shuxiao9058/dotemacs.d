;;; core/core-evil.el -*- lexical-binding: t; -*-

;; ;; Fix cursor for Evil mode
;; (defun my-send-string-to-terminal (string)
;;   (unless (display-graphic-p) (send-string-to-terminal string)))

;; (defun my-evil-terminal-cursor-change ()
;;   (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
;;     (add-hook 'evil-insert-state-entry-hook (lambda ()
;; 					      (message "enter insert mode")
;; 					      (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
;;     (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
;;   (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
;;     (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
;;     (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

;; (eval-after-load 'evil
;;   (progn
;;     (add-hook 'after-make-frame-functions
;; 	      (lambda (frame) (my-evil-terminal-cursor-change)))
;;     (my-evil-terminal-cursor-change)))

(defun sp/evil-hook ()
  (dolist (mode '(dashboard-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :straight t
  :ensure t
  :commands evil-normalize-keymaps
  :hook (evil-mode . sp/evil-hook)
  :custom
  ;; Change cursor color depending on mode
  (evil-emacs-state-cursor `("red" hbar))     ; _
  (evil-normal-state-cursor `("green" box))   ; █
  (evil-visual-state-cursor `("orange" box))  ; █
  (evil-insert-state-cursor `("red" bar))     ; ⎸
  (evil-replace-state-cursor `("red" bar))
  (evil-operator-state-cursor `("red" hollow))
  (evil-motion-state-cursor `("orange" box))  ; █
  ;; set bar cursor for evil-emacs mode
  (evil-emacs-state-cursor '(bar))
  (evil-want-C-u-scroll t)
  (evil-search-module 'evil-search)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-i-jump nil)
  :general
  (nmap "q" nil ;; q quit, not evil-record-macro
    "Q" #'evil-record-macro)
  :config
  (evil-mode +1)
  (add-to-list 'evil-insert-state-modes 'shell-mode)
  (add-to-list 'evil-insert-state-modes 'dashboard-mode)
  (add-to-list 'evil-insert-state-modes 'git-timemachine-mode)

  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (recenter))

  (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
    (recenter))
  )

   ;;; This advice is related to issue: https://github.com/hlissner/doom-emacs/issues/2493
(advice-add #'turn-on-evil-mode :before
            (lambda (&optional args)
              (when (eq major-mode 'fundamental-mode)
                (hack-local-variables))))

(use-package evil-terminal-cursor-changer
  :straight t
  :unless IS-GUI
  :ensure t
  :after evil
  ;; :custom
  :config
  (evil-terminal-cursor-changer-activate) ; or (etcc-on)
  )

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
  :after evil
  :commands evil-exchange
  :straight t
  :config
  (evil-exchange-cx-install))

;;; Press “%” to jump between matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair of tags.
(use-package evil-matchit
  :straight t
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
  :straight t
  :commands (evil-snipe-mode evil-snipe-override-mode
			     evil-snipe-local-mode evil-snipe-override-local-mode)
  :hook (prog-mode . evil-snipe-mode)
  :custom
  (evil-snipe-smart-case t)
  (evil-snipe-scope 'line)
  (evil-snipe-repeat-scope 'visible)
  (evil-snipe-spillover-scope 'visible)
  (evil-snipe-char-fold t)
  (evil-snipe-disabled-modes '(magit-mode elfeed-show-mode elfeed-search-mode))
  (evil-snipe-aliases '((?\[ "[[{(]")
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

(use-package evil-arglist
  :straight (evil-arglist :host github
			  :repo "dzop/evil-arglist")
  )

(use-package evil-args
  :straight t
  :after evil
  :commands (evil-inner-arg evil-outer-arg
			    evil-forward-arg evil-backward-arg
			    evil-jump-out-args)
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (set (make-local-variable 'evil-args-delimiters) '(" " "\t"))))
  :general
  (general-itomap "a" #'evil-inner-arg)
  (general-otomap "a" #'evil-outer-arg))

(use-package evil-visualstar
  :straight t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-textobj-entire
  :straight t
  :after evil
  :general
  (general-itomap "e" #'evil-entire-entire-buffer)
  (general-otomap "e" #'evil-entire-entire-buffer))

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
  :after pbcopy
  :straight (evil-osx-clipboard
	     :host github
	     :repo "stroxler/evil-osx-clipboard.el"
             :files (:defaults))
  :config
  (evil-osx-clipboard/set-osx-defaults)
  )

;; Evil, probably the best thing since Evil.
(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :demand
  ;; :init
  :config
  ;; enabled for all modes provided by default by evil-collection
  ;; change this if wanted to enable for specific modes
  (evil-collection-init)
  ;; magit-evil
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'magit-log-mode 'motion)
  (evil-set-initial-state 'magit-diff-mode 'normal)
  (evil-set-initial-state 'magit-wassup-mode 'motion)
  (evil-set-initial-state 'magit-mode 'motion)
  (evil-set-initial-state 'git-rebase-mode 'motion)
  (eval-after-load 'git-rebase
    `(progn
       (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
	 (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
	   (setcar desc (cdr key))))
       )
    )
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-company-use-tng nil)
  :general
  (:states '(evil-magit-state)
	   :keymaps '(git-rebase-mode-map)
	   "gj" #'git-rebase-move-line-down
	   "gk" #'git-rebase-move-line-up)
  )

(provide 'core-evil)
;;; core-evil.el ends here
