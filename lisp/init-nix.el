;;; lisp/init-nix.el -*- lexical-binding: t; -*-

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function 'nix-indent-line))

(provide 'init-nix)
;;; init-nix.el ends here
