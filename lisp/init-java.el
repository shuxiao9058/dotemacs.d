;;; lisp/init-java.el -*- lexical-binding: t; -*-

;; LSPJavaPac
(use-package lsp-java
  :straight t
  :after lsp-mode
  ;; :if *mvn*
  ;; :config
  ;; (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/server/" user-emacs-directory))
  (lsp-java-workspace-dir (expand-file-name "eclipse.jdt.ls/workspace/" user-emacs-directory)))

(provide 'init-java)
;;; init-java.el ends here
