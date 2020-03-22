;;; lisp/init-vc.el -*- lexical-binding: t; -*-


(use-package magit
    :straight t
    :commands (magit-file-delete magit-status magit-checkout)
    :hook (magit-pop-mode . hide-mode-line-mode)
    :init
    ;; Must be set early to prevent ~/.emacs.d/transient from being created
    (setq transient-levels-file  (concat poly-etc-dir "transient/levels")
          transient-values-file  (concat poly-etc-dir "transient/values")
          transient-history-file (concat poly-etc-dir "transient/history"))

    (setq magit-revert-buffers 'silent
          magit-push-always-verify nil
          git-commit-summary-max-length 70
          ;; use colored graph lines. Could be a performance issue.
	  magit-log-arguments (quote ("-n256" "--graph" "--decorate" "--color" "--stat"))
	  magit-diff-use-overlays nil
	  magit-use-overlays nil
	  magit-auto-revert-mode t
	  git-commit-finish-query-functions nil)
    ;; ;; Use flyspell in the commit buffer
    ;; (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
    ;; (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
    :config
    (when IS-MAC
      (setq magit-git-executable "/usr/local/bin/git"))
    )

(use-package magit-gitflow
    :straight t
    :after magit
    :commands magit-gitflow-popup
    :hook (magit-mode . turn-on-magit-gitflow)
    )

;; git-gutter-plus - View, stage and revert Git changes from the buffer (inspired by package of same name from vim)
(use-package git-gutter+
    :straight t
    :diminish git-gutter+-mode
    :demand t
    :bind (("C-c g n" . git-gutter+-next-hunk)
           ("C-c g p" . git-gutter+-previous-hunk))
    :config
    (progn
      (global-git-gutter+-mode)
      ))

(use-package git-gutter-fringe+ :straight t)

;; git-messenger - Provides a function popup commit message at current line (port of package of same name from vim)
(use-package git-messenger
    :straight t
    :bind ("C-c g p" . git-messenger:popup-message)
    :init
    (custom-set-variables
     '(git-messenger:use-magit-popup t))
    (setq git-messenger:show-detail t)
    :config
    (progn
      (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

;; git-timemachine - Step through historic versions of a git controlled file
(use-package git-timemachine
    :straight t
    :bind ("C-c g t" . git-timemachine-toggle))

;; gitignore-mode - Major mode for various Git configuration files
(use-package gitignore-mode :straight t)

;; browse-at-remote - Browse target page on github/gitlab/bitbucket
(use-package browse-at-remote
    :straight t
    :bind ("C-c g b" . browse-at-remote/browse))

(use-package evil-magit
    :straight t
    :ensure t
    :after magit evil
    :hook (magit-mode . evil-magit-init)
    :init
    (progn
      (evil-set-initial-state 'magit-log-edit-mode 'insert)
      (evil-set-initial-state 'git-commit-mode 'insert)
      (evil-set-initial-state 'magit-commit-mode 'insert)
      ;; (evil-set-initial-state 'magit-commit-mode 'motion)
      (evil-set-initial-state 'magit-log-mode 'motion)
      (evil-set-initial-state 'magit-wassup-mode 'motion)
      (evil-set-initial-state 'magit-mode 'motion)
      (evil-set-initial-state 'git-rebase-mode 'motion)
      )
    )

;; based on http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(defun magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))


(provide 'init-vc)
;;; init-vc.el ends here
