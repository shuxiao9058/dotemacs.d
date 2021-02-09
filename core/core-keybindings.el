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
  :straight (general
	     :host github
	     :repo "noctuid/general.el"
	     :files ("general.el")
	     )
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

  ;; https://github.com/noctuid/general.el/issues/126

  (general-create-definer poly--default-leader
    :states '(normal visual)
    :prefix leader-key
    :keymaps 'override)

  (general-create-definer poly-global-leader
    :states general-non-normal-states
    :prefix leader-key-non-normal
    :keymaps 'override)

  (defmacro leader-def (&rest args)
    "Define for both default leader and global leader."
    (declare (indent defun))
    `(progn
       (poly--default-leader
	 ,@args)
       (poly-global-leader
	 ,@args)))

  ;; (general-create-definer leader-def
  ;;   :states '(normal insert motion visual emacs dashboard) ;; '(normal visual insert emacs)
  ;;   ;; :prefix-name "SPC"
  ;;   :non-normal-prefix leader-key-non-normal
  ;;   :prefix leader-key
  ;;   :keymaps 'override
  ;;   ) ;; normal visual insert emacs

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
    ;; "C-x o"
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


(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; Yank text to clipboard
(cond
 ;; OS X
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq save-to-clipboard-cmd "pbcopy")
    (setq paste-from-clipboard-cmd "pbpaste")
    )
  )
 ;; Linux
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (setq save-to-clipboard-cmd "xsel -i -b")
    (setq paste-from-clipboard-cmd "xsel -o -b")
    )
  )
 )

(defun copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) save-to-clipboard-cmd)
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string paste-from-clipboard-cmd))
    )
  )
;; (defun copy-to-clipboard()
;;   "Copies selection to x-clipboard."
;;   (interactive)
;;   (if (display-graphic-p)
;;       (progn
;;         (if (use-region-p)
;;             (progn
;;               (evil-yank (region-beginning) (region-end) t (evil-use-register ?+))
;;               (message "Yanked region to clipboard!")
;;               (deactivate-mark))
;;           (message "No region active; can't yank to clipboard!"))
;;         )
;;     ))

;; (defun paste-from-clipboard ()
;;   "Pastes from x-clipboard."
;;   (interactive)
;;   (evil-paste-after 1 (evil-use-register ?+))
;;   )

;; (defun my/evil-paste ()
;;   (interactive)
;;   (evil-visual-paste 1)
;;   (right-char))

(general-define-key
 :keymaps 'override
 "s-c" #'copy-to-clipboard
 "s-v" #'paste-from-clipboard
 )

;; (general-define-key :states '(visual insert normal)
;; 		    :keymaps 'override
;; 		    "s-v" 'my/evil-paste)

(provide 'core-keybindings)
;;; core-keybindings.el ends here
