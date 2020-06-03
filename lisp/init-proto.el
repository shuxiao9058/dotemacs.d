;;; lisp/init-proto.el -*- lexical-binding: t; -*-

(use-package protobuf-mode
  :straight t
  :hook (protobuf-mode . (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil t)))
  :mode (("\\.proto\\'" . protobuf-mode))
  :ensure t)

(provide 'init-proto)
;;; init-proto.el ends here
