;;; lisp/init-projectile.el -*- lexical-binding: t; -*-

(use-package project
  :straight nil
  :custom
  (project-vc-ignores
   '("vendor/" "*.elc" "*.a"
     "tmp" "dist" "coverage"
     ".idea" ".vscode"
     ".ensime_cache" ".eunit"
     ".git" ".hg" ".fslckout"
     "_FOSSIL_" ".bzr" "_darcs"
     ".tox" ".svn"
     ".stack-work" ".ccls-cache" ".cache" ".clangd")
   '(".log" ".vs" "node_modules"))
  )

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
