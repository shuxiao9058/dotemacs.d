;;; lisp/init-vc.el -*- lexical-binding: t; -*-


(use-package magit
  :straight t
  :commands (magit-status magit-checkout)
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-checkout)
         ("C-c g B" . magit-blame))
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
  ;; Use flyspell in the commit buffer
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

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
  (use-package git-gutter-fringe+ :straight t)))

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


(provide 'init-vc)