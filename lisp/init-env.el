;;; lisp/init-env.el -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
    :straight t
    :demand t
    :if IS-MAC
    :config
    (exec-path-from-shell-initialize)
    )

(provide 'init-env)

;;; init-env.el ends here