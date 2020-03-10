;;; lisp/init-projectile.el -*- lexical-binding: t; -*-

(use-package swiper
  :straight t
  :commands swiper
  :after ivy)

(use-package counsel
  :straight t
  :after ivy
  :defer t
  :config
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)
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