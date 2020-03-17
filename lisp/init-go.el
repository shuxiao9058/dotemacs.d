;;; lisp/init-go.el -*- lexical-binding: t; -*-

(use-package go-mode
    :straight t
    :after company
    :ensure t
    :commands (godoc gofmt gofmt-before-save)
    :hook (befor-save . gofmt-before-save)
    :config
    (setq gofmt-command "goimports")
    ;; (add-hook 'before-save-hook #'gofmt-before-save)
    ;; (setq-local company-backends
    ;; 		(let ((b #'company-tabnine))
    ;; 		  (cons b (remove b company-backends))))
    )

(use-package gorepl-mode
    :straight t
    :after go-mode
    :commands gorepl-run-load-current-file)

;; (use-package flycheck-golangci-lint
;;   :straight t
;;   :after go-mode
;;   :hook (go-mode . flycheck-golangci-lint-setup)
;;   :config
;;   (setenv "GO111MODULE" "on")
;;   (setq flycheck-golangci-lint-enable-all t)
;;   ;; (setq flycheck-golangci-lint-config (concat (expand-file-name "~") "/.config/golangci-lint/golangci.yml"))
;;   (setq flycheck-golangci-lint-fast t)
;;   (setq flycheck-golangci-lint-tests t))

;; (use-package company-go
;;     :straight t
;;   :after go-mode
;;   :config
;;   (setq company-go-show-annotation t))

(use-package go-eldoc
    :straight t
    :after go-mode
    :ensure t
    :commands go-eldoc-setup
    :init
    (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-rename
    :straight t
    :after go-mode
    :ensure t
    :commands go-rename)

(use-package go-guru
    :straight t
    :after go-mode
    :ensure t
    :commands go-guru-hl-identifier-mode
    :init
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(provide 'init-go)
;;; init-go.el ends here
