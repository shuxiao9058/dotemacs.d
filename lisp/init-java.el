;;; lisp/init-java.el -*- lexical-binding: t; -*-

;; LSPJavaPac
(use-package lsp-java
  :after lsp-mode
  ;; :if *mvn*
  ;; :config
  ;; (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))

(provide 'init-java)
;;; init-java.el ends here