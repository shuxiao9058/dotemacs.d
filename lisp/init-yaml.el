;;; lisp/init-yaml.el -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t
  :mode
  (("\\.yml\\’" . yaml-mode)
   ("\\.yaml\\’" . yaml-mode)))

(provide 'init-yaml)
;;; init-yaml.el ends here
