;;; lisp/init-recentf.el -*- lexical-binding: t; -*-

(use-package recentf
    :straight t
  ;; Keep track of recently opened files
  ; :defer-incrementally easymenu tree-widget timer
  :after after-find-file
  :commands recentf-open-files
  :config
  (defun poly--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers
        '(substring-no-properties
          poly--recent-file-truename
          abbreviate-file-name)
        recentf-save-file (concat poly-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  (add-hook! '(poly-switch-window-hook write-file-functions)
    (defun poly--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  (add-hook! 'dired-mode-hook
    (defun poly--recentf-add-dired-directory-h ()
      "Add dired directory to recentf file list."
      (recentf-add-file default-directory)))

  (when poly-interactive-mode
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (quiet! (recentf-mode +1))))

(provide 'init-recentf)
;;; init-recentf.el ends here