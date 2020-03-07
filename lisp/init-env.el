;;; lisp/init-env.el -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  :demand t
  :if IS-MAC
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  )

(provide 'init-env)

;;; init-env.el ends here
