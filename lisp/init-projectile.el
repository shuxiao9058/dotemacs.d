;;; lisp/init-projectile.el -*- lexical-binding: t; -*-

(use-package projectile
  :straight t
  :commands projectile-global-mode
  :delight
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" poly-cache-dir)
	projectile-known-projects-file (concat poly-cache-dir "projectile-bookmarks.eld")
	projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :commands counsel-projectile-mode
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(provide 'init-projectile)

;;; init-projectile.el ends here
