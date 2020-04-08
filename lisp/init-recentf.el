;;; lisp/init-recentf.el -*- lexical-binding: t; -*-

(use-package recentf
  :straight t
  :commands (recentf-mode
	     recentf-add-file
	     recentf-apply-filename-handlers
	     recentf-open-files)
  :config
  (setq recentf-max-saved-items nil
	recentf-max-menu-items 60
	;; disable recentf-cleanup on Emacs start, because it can cause
	recentf-auto-cleanup 'never ;; problems with remote files
	;; recentf-auto-cleanup 600
	recentf-filename-handlers '(file-truename abbreviate-file-name)
	recentf-save-file (expand-file-name "recentf" poly-cache-dir)
	recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)
	;; exclude ** from recentfiles buffer
	recentf-exclude '("\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
			  "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
			  "^/var/folders\\.*" "\\.git/config" "\\.git/COMMIT_EDITMSG"
			  "COMMIT_MSG"
			  "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
			  "github.*txt$"
			  "COMMIT_EDITMSG\\'"
			  ".*-autoloads\\.el\\'"
			  "recentf"
			  ".*pang$" ".*cache$"
			  "[/\\]\\.elpa/")
	)

  (recentf-mode +1)
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'recentf-cleanup))
  )

(provide 'init-recentf)
;;; init-recentf.el ends here
