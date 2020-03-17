;;; lisp/init-vc.el -*- lexical-binding: t; -*-


(use-package magit
    :straight t
    :commands (magit-file-delete magit-status magit-checkout)
    :after evil
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
    (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

    ;;   ;; Add --tags switch
    ;;  (transient-append-suffix 'magit-fetch "-p"
    ;;   '("-t" "Fetch all tags" ("-t" "--tags")))
    ;;  (transient-append-suffix 'magit-pull "-r"
    ;;   '("-a" "Autostash" "--autostash"))
    :config
    ;; ;; initialize magit-status and magit-commit in same mode as everything else
    ;; (evil-set-initial-state 'magit-status-mode evil-default-state)
    ;; (evil-set-initial-state 'magit-commit-mode evil-default-state)
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

(use-package evil-magit
    :straight t
    :after magit
    :init
    ;; (setq evil-magit-state 'normal
    ;;       evil-magit-use-z-for-folds t)
    :config
    (progn
      (general-unbind magit-mode-map
	  ;; Replaced by z1, z2, z3, etc
	  "M-1" "M-2" "M-3" "M-4"
	  "1" "2" "3" "4"
	  "0") ; moved to g=

      (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
      (evil-define-key* '(normal visual) magit-mode-map
			"%"  'magit-gitflow-popup
			"zz" 'evil-scroll-line-to-center
			"g=" 'magit-diff-default-context)

      (general-def 'normal
	  (magit-status-mode-map
	   magit-stash-mode-map
	   magit-revision-mode-map
	   magit-diff-mode-map)
	[tab] #'magit-section-toggle)

      (eval-after-load 'git-rebase
	`(progn
	   (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
	     (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
               (setcar desc (cdr key))))
	   (evil-define-key* evil-magit-state git-rebase-mode-map
			     "gj" #'git-rebase-move-line-down
			     "gk" #'git-rebase-move-line-up)
	   )
	)
      (evil-set-initial-state 'magit-log-edit-mode 'insert)
      (evil-set-initial-state 'git-commit-mode 'insert)
      ;; (evil-set-initial-state 'magit-commit-mode 'insert)
      (evil-set-initial-state 'magit-commit-mode 'motion)
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

(eval-after-load 'magit
  `(progn
     ;; Temporary workaround for +magit/quit hang with lots of buffers
     (general-define-key :keymaps 'magit-status-mode-map
			 [remap magit-mode-bury-buffer] #'magit-kill-buffers)
     )
  )

;; Close transient with ESC
(define-key transient-map [escape] #'transient-quit-one)

;; ;;; functions
;; ;; key binding for magit status
;; (defun magit-status-and-focus-unstaged ()
;;   "Opens the magit-status view and focuses the cursor on the first unstaged file."
;;   (interactive)
;;   (call-interactively 'magit-status)
;;   (magit-jump-to-unstaged)
;;   (magit-goto-next-section))

(provide 'init-vc)
;;; init-vc.el ends here
