;;; lisp/init-recentf.el -*- lexical-binding: t; -*-

(use-package recentf
  :straight nil
  :commands (recentf-mode
	     recentf-add-file
	     recentf-apply-filename-handlers
	     recentf-open-files)
  ;; :defines no-littering-etc-directory no-littering-var-directory quelpa-packages-dir
  ;; :after no-littering
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  (recentf-auto-cleanup 'never)
  ;; `recentf-add-file' will apply handlers first, then call `string-prefix-p'
  ;; to check if it can be pushed to recentf list.
  (recentf-filename-handlers '(abbreviate-file-name))
  ;; ;; recentf-auto-cleanup 600
  ;; recentf-filename-handlers '(file-truename abbreviate-file-name)
  (recentf-max-menu-items 60)
  (recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))
  ;; exclude ** from recentfiles buffer
  (recentf-exclude `(;; ,@(cl-loop  for f in `(
					   ;; ,package-user-dir
                                           ;; ,quelpa-packages-dir
                                           ;; ,no-littering-var-directory
                                           ;; ,no-littering-etc-directory)
                        ;;        collect (abbreviate-file-name f))
		     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
		     "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
		     "^/var/folders\\.*" "\\.git/config" "\\.git/COMMIT_EDITMSG"
		     "COMMIT_MSG"
		     "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
		     "github.*txt$"
		     "COMMIT_EDITMSG\\'"
		     ".*-autoloads\\.el\\'"
		     "recentf"
		     ".*pang$" ".*cache$"
		     "[/\\]\\.elpa/"
                     ;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     ;; "COMMIT_EDITMSG\\'"
		     ))
  ;; :config
  ;; (setq recentf-max-saved-items nil
  ;; 	recentf-max-menu-items 60
  ;; 	recentf-auto-cleanup 'never ;; problems with remote files
  ;; 	;; recentf-auto-cleanup 600
  ;; 	recentf-filename-handlers '(file-truename abbreviate-file-name)
  ;; 	recentf-save-file (expand-file-name "recentf" poly-cache-dir)
  ;; 	)

  ;; (recentf-mode +1)
  ;; (unless noninteractive
  ;;   (add-hook 'kill-emacs-hook #'recentf-cleanup))
  )

(provide 'init-recentf)
;;; init-recentf.el ends here
