;;; core/core-keybindings.el -*- lexical-binding: t; -*-

(defconst leader-key "SPC"
  "The leader prefix key for global commands.")

(defconst leader-key-non-normal "M-m"
  "The leader prefix key for global commands in emacs and insert states.")

(defconst local-leader-key "SPC m"
  "The local leader prefix key for major mode specific commands.")

(defconst local-leader-key-non-normal "M-,"
  "The local leader prefix key for major mode specific commands in emacs and insert states.")

(use-package which-key
  :straight t
  :defer 1
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.3)
  (which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 4)
  (which-key-side-window-slot -10)
  (which-key-show-early-on-C-h t)
  (which-key-allow-evil-operators t)
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
    (insert (shell-command-to-string paste-from-clipboard-cmd))))

(bind-key "s-c" #'copy-to-clipboard)
(bind-key "s-v" #'paste-from-clipboard)
(bind-key "s-SPC" #'set-mark-command)
(unbind-key "C-z")

(provide 'core-keybindings)
;;; core-keybindings.el ends here
