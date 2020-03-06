;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

;; lsp-mode configuration
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
     (c++-mode . lsp-deferred)
     (js-mode . lsp-deferred)
     (js-jsx-mode . lsp-deferred)
     (haskell-mode . lsp-deferred)
     (rust-mode . lsp-deferred)))

(use-package lsp-haskell
  :straight t)

(use-package lsp-ui
  :straight t
  :init (setq lsp-prefer-flymake nil)
  :commands lsp-ui-mode
  :hook ((lsp-mode-hook . lisp-ui-mode)))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :straight t
  :commands company-mode
  :hook ((after-init . global-company-mode))
  :general
  (:states '(insert normal) "<backtab>" 'company-complete))

(use-package company-lsp
  :straight t
  :after company
  :config
  (push 'company-lsp company-backends))

(use-package company-quickhelp
  :straight t
  :after company
  :config
  (company-quickhelp-mode))

(provide 'init-lsp)