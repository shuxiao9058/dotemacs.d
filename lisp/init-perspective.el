;;; lisp/init-perspective.el -*- lexical-binding: t; -*-

(use-package perspective
  :straight t
  :demand t
  :commands
  (persp-mode)
  :config
  (persp-mode))

(use-package persp-projectile
  :straight t
  :general
  ('(normal visual evilified) :prefix "SPC" :non-normal-prefix "C-SPC"
   "pl" #'projectile-persp-switch-project))

(provide 'init-perspective)

;;; init-perspective.el ends here
