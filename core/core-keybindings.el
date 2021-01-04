;;; core/core-keybindings.el -*- lexical-binding: t; -*-

(defconst leader-key "SPC"
  "The leader prefix key for global commands.")

(defconst leader-key-non-normal "M-m"
  "The leader prefix key for global commands in emacs and insert states.")

(defconst local-leader-key "SPC m"
  "The local leader prefix key for major mode specific commands.")

(defconst local-leader-key-non-normal "M-,"
  "The local leader prefix key for major mode specific commands in emacs and insert states.")

(use-package general
  :straight t
  :ensure t
  :demand t
  :commands (general-define-key general-create-definer general-override-mode general-evil-setup general--simulate-keys)
  :functions space-leader-def
  :init
  (setq general-override-states '(insert
				  emacs
				  hybrid
				  normal
				  visual
				  motion
				  operator
				  replace))
  (general-evil-setup t) ;;; vmap,nmap,etc...
  :config
  ;;
  ;; General + leader/localleader keys
  ;; Removing emacs state from non-normal list allow the use of SPC
  (delete 'emacs general-non-normal-states)
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-create-definer leader-def
    :states '(normal insert motion visual emacs dashboard) ;; '(normal visual insert emacs)
    ;; :prefix-name "SPC"
    :non-normal-prefix leader-key-non-normal
    :prefix leader-key
    :keymaps 'override
    ) ;; normal visual insert emacs
  (leader-def
    "" '(nil :which-key "leader prefix"))
  (general-create-definer local-leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix local-leader-key
    :non-normal-prefix local-leader-key-non-normal)
  (local-leader-def
    "" '(nil :which-key "local-leader prefix"))
  (nvmap "," (general-simulate-key "SPC m"))
  (general-unbind "C-x b"
    "C-x C-f"
    "C-x C-w"
    "C-x C-s"
    "C-x o"
    ;; disable C-z
    "C-z"
    "M-x"
    "M-:"
    "C-\\"
    ))

;;
;;; Packages
(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 4
	which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(provide 'core-keybindings)
;;; core-keybindings.el ends here
