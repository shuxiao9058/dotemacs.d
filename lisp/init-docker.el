
;;; lisp/init-docker.el -*- lexical-binding: t; -*-

(use-package dockerfile-mode
  :straight t
  :config
  ;; (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(provide 'init-docker)
;;; init-docker ends here
