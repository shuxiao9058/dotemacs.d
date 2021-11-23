;;; lisp/init-vc.el -*- lexical-binding: t; -*-

(use-package with-editor
    :straight t
    :ensure t)

(use-package emacsql
    :straight t
    :ensure t)

(use-package magit
    :straight t
    :commands (magit-file-delete magit-status magit-checkout)
    :hook (magit-pop-mode . hide-mode-line-mode)
    :custom
    ;; (magit-refresh-verbose t) ;; debug only
    ;; (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (magit-revert-buffers 'silent)
    (git-commit-summary-max-length 50)
    (magit-log-section-commit-count 5)
    (magit-diff-options (quote ("--minimal" "--patience")))
    (magit-tag-arguments (quote ("--annotate" "--sign")))
    (magit-merge-arguments (quote ("--no-ff")))
    (magit-rebase-arguments (quote ("--autostash")))
    ;; use colored graph lines. Could be a performance issue.
    (magit-log-arguments (quote ("-n64" "--graph" "--decorate" "--color" "--stat")))
    (magit-diff-use-overlays nil)
    (magit-use-overlays nil)
    (magit-auto-revert-mode nil)
    (git-rebase-auto-advance  t)
    (magit-stage-all-confirm nil)
    (magit-commit-squash-commit 'marked-or-curren)
    (magit-push-always-verify ni) ;; cuz it says so
    (magit-diff-refine-hunk 'all)
    (git-commit-finish-query-functions nil)
    (magit-log-section-commit-count 10)
    (magit-log-section-arguments '("--graph" "--decorate" "--color"))
    ;; (magit-git-executable "/usr/local/bin/git")
    :init
    ;; Must be set early to prevent ~/.emacs.d/transient from being created
    (setq transient-levels-file  (concat poly-etc-dir "transient/levels")
	  transient-values-file  (concat poly-etc-dir "transient/values")
	  transient-history-file (concat poly-etc-dir "transient/history"))

    ;; Have magit-status go full screen and quit to previous
    ;; configuration.  Taken from
    ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
    ;; and http://irreal.org/blog/?p=2253
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice magit-quit-window (after magit-restore-screen activate)
      (jump-to-register :magit-fullscreen))
    ;; (setq
    ;; ;; Use flyspell in the commit buffer
    ;; (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
    :config
    (setq magit-status-sections-hook
	  '(
	    magit-insert-status-headers
	    magit-insert-merge-log
	    magit-insert-rebase-sequence
	    magit-insert-am-sequence
	    magit-insert-sequencer-sequence
	    magit-insert-bisect-output
	    magit-insert-bisect-rest
	    magit-insert-bisect-log
	    magit-insert-untracked-files
	    magit-insert-unstaged-changes
	    magit-insert-staged-changes
	    ;; magit-insert-unpushed-cherries
	    magit-insert-stashes
	    ;; magit-insert-recent-commits
	    ;; magit-insert-unpulled-from-pushremote
	    magit-insert-unpushed-to-upstream
	    magit-insert-unpushed-to-pushremote
	    magit-insert-unpulled-from-upstream
	    ))

    (setq magit-status-headers-hook
	  '(
	    ;; magit-insert-repo-header
	    ;; magit-insert-remote-header
	    magit-insert-error-header
	    magit-insert-diff-filter-header
	    magit-insert-head-branch-header
	    magit-insert-upstream-branch-header
	    magit-insert-push-branch-header
	    magit-insert-tags-header
	    ))

    ;; Opening repo externally
    (defun poly/parse-repo-url (url)
      "convert a git remote location as a HTTP URL"
      (if (string-match "^http" url)
	  url
	(replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
				  (concat (if (string-match "17usoft.com" url) "http" "https") "://\\2/\\3")
				  url)))
    (defun poly/magit-open-repo ()
      "open remote repo URL"
      (interactive)
      (let ((url (magit-get "remote" "origin" "url")))
	(progn
	  (browse-url (poly/parse-repo-url url))
	  (message "opening repo %s" url))))

    (defun m/magit-display-buffer-traditional (buffer)
      "Like magit-display-buffer-traditional, but re-uses window for status mode, too."
      (display-buffer
       buffer (if (not (memq (with-current-buffer buffer major-mode)
			     '(magit-process-mode
			       magit-revision-mode
			       magit-diff-mode
			       magit-stash-mode
			       magit-status-mode)))
		  '(display-buffer-same-window)
		nil)))

    (setq magit-display-buffer-function 'm/magit-display-buffer-traditional)

    (defun m/magit-reset-author (&optional args)
      "Resets the authorship information for the last commit"
      (interactive)
      (magit-run-git-async "commit" "--amend" "--no-edit" "--reset-author"))

    ;; (magit-define-popup-action 'magit-commit-popup
    ;;   ?R "Reset author" 'm/magit-reset-author)
    (transient-append-suffix 'magit-commit
	"S"
      '("R" "Reset author" m/magit-reset-author))
    :bind
    (:map transient-base-map
	  ("q" . transient-quit-one)
	  ("<escape>" . transient-quit-one))
    (:map transient-edit-map
	  ("q" . transient-quit-one)
	  ("<escape>" . transient-quit-one))
    (:map transient-sticky-map
	  ("q" . transient-quit-one)
	  ("<escape>" . transient-quit-one)))

(use-package magit-gitflow
    :straight t
    :after magit
    :commands magit-gitflow-popup
    :hook (magit-mode . turn-on-magit-gitflow)
    )

;; Show TODOs in magit
(use-package magit-todos
    :straight t
    :diminish
    :after magit
    :config
    (magit-todos-mode))

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
(use-package git-modes :straight t)

;; browse-at-remote - Browse target page on github/gitlab/bitbucket
(use-package browse-at-remote
    :straight t
    :bind ("C-c g b" . browse-at-remote/browse))

;; based on http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(defun magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; required by forge
(use-package yaml
    :straight t)

(use-package forge
    :straight t
    :after (magit yaml)
    :commands forge-create-pullreq forge-create-issue
    :custom
    (forge-database-file (expand-file-name "forge/forge-database.sqlite" poly-etc-dir))
    (custom-set-variables '(forge-post-mode-hook '(visual-line-mode)))
    (forge-bug-reference-hooks
     '(git-commit-setup-hook magit-mode-hook))
    :config
    (setq forge-alist
	  (append forge-alist
		  '(("git.17usoft.com" "git.17usoft.com/api/v4" "git.17usoft.com" forge-gitlab-repository)
		    ("github.com" "api.github.com" "github.com" forge-github-repository))))
    ;; ;; remove some hooks for magit performance-s
    ;; (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
    ;; (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
    )

(use-package ghub
    :straight t
    :after (magit forge)
    :custom
    (ghub-insecure-hosts '("git.17usoft.com/api/v4")))

(use-package smerge-mode
    :straight t
    :ensure t
    :diminish
    :commands (smerge-mode
               smerge-auto-leave
               smerge-next
               smerge-prev
               smerge-keep-base
               smerge-keep-upper
               smerge-keep-lower
               smerge-keep-all
               smerge-keep-current
               smerge-keep-current
               smerge-diff-base-upper
               smerge-diff-upper-lower
               smerge-diff-base-lower
               smerge-refine
               smerge-ediff
               smerge-combine-with-next
               smerge-resolve
               smerge-kill-current)
    :after (hydra magit)
    :hook ((find-file . (lambda ()
                          (save-excursion
                            (goto-char (point-min))
                            (when (re-search-forward "^<<<<<<< " nil t)
                              (smerge-mode 1)))))

	   ( magit-diff-visit-file . (lambda ()
				       (when smerge-mode
					 (smerge-hydra/body))))))

(use-package vdiff
    :straight t)

(provide 'init-vc)
;;; init-vc.el ends here
