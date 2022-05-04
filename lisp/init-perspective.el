;;; lisp/init-perspective.el -*- lexical-binding: t; -*-

(use-package perspective
  :straight t
  :demand t
  :commands
  (persp-mode)
  :config
  (persp-mode)
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  )

(use-package persp-projectile
  :straight t
  )

(provide 'init-perspective)

;;; init-perspective.el ends here
