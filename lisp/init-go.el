;;; lisp/init-go.el -*- lexical-binding: t; -*-

(use-package go-mode
  :straight t
  :ensure t
  :commands (godoc gofmt gofmt-before-save)
  ;; :after (format-all)
  :init
  (defun lsp-go-install-save-hooks()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (setq-local lsp-enable-on-type-formatting t))
  (eval-after-load 'lsp
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
  )

(use-package gorepl-mode
  :straight t
  :after go-mode
  :commands gorepl-run-load-current-file)

;; Install: See https://github.com/golangci/golangci-lint#install
(use-package flycheck-golangci-lint
  :straight t
  :after (flycheck go-mode)
  ;; :hook (go-mode . flycheck-golangci-lint-setup)
  :hook (go-mode . (lambda ()
                     "Enable golangci-lint."
                     (setq flycheck-disabled-checkers '(go-gofmt
                                                        go-golint
                                                        go-vet
                                                        go-build
                                                        go-test
                                                        go-staticcheck
                                                        go-errcheck))
                     (flycheck-golangci-lint-setup)))
  :defines flycheck-disabled-checkers
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

;; (use-package go-mod-mode
;;     :straight (:host github :repo "zkry/go-mod-mode")
;;     :ensure t
;;     :mode (("go\\.mod\\'" . go-mod-mode)))

(use-package go-tag
  :straight t
  :bind (:map go-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-gen-test
  :straight t
  :bind (:map go-mode-map
              ("C-c t g" . go-gen-test-dwim)))

(use-package gotest
  :straight t
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)
              ("C-c t j" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t x" . go-run)))

(use-package go-playground
  :straight t
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)
;;; init-go.el ends here
