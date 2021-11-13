;;; lisp/init-go.el -*- lexical-binding: t; -*-

(use-package go-mode
    :straight t
    :ensure t
    :commands (godoc gofmt gofmt-before-save)
    :config
    (defun lsp-go-install-save-hooks ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)
      )

    (eval-after-load-all '(lsp doom-modeline-env)
			 (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
    ;; (setenv "GOPROXY" "")
    )

(use-package gorepl-mode
    :straight t
    :after go-mode
    :commands gorepl-run-load-current-file)

(use-package flycheck-golangci-lint
    :straight t
    :after go-mode
    :hook (go-mode . flycheck-golangci-lint-setup)
    :disabled
    :custom
    ;; (flycheck-golangci-lint-enable-all t)
    ;; (flycheck-golangci-lint-fast t)
    (flycheck-golangci-lint-config
     (expand-file-name "golangci.yml" "~/.config/golangci-lint"))
    ;; (flycheck-golangci-lint-tests t)
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
    )

(use-package go-eldoc
    :straight t
    :after go-mode
    :ensure t
    :disabled
    :commands go-eldoc-setup
    :init
    (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-rename
    :straight t
    :after go-mode
    :disabled
    :ensure t
    :commands go-rename)

(use-package go-guru
    :straight t
    :after go-mode
    :disabled
    :ensure t
    :commands go-guru-hl-identifier-mode
    :init
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package go-mod-mode
    :straight (:host github :repo "zkry/go-mod-mode")
    :ensure t
    :mode (("go\\.mod\\'" . go-mod-mode)))

(provide 'init-go)
;;; init-go.el ends here
