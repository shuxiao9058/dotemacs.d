;;; lisp/init-go.el -*- lexical-binding: t; -*-

(use-package go-mode
  :straight t
  :ensure t
  :commands (godoc gofmt gofmt-before-save)
  :hook (befor-save . gofmt-before-save)
  :config
  (setq gofmt-command "goimports")
  (setenv "GO111MODULE" "on")
					; (set-lookup-handlers! 'go-mode
					;     :definition #'go-guru-definition
					;     :references #'go-guru-referrers
					;     :documentation #'godoc-at-point)
  ;; :general
  ;; (nmap :keymaps '(go-mode-map)
  ;;   "gd" 'godef-jump
  ;;   "SPC d" 'godoc-at-point)
  ;; (leader-def :keymaps '(go-mode-map)
  ;;   "c" '(:ignore t :wk "code")
  ;;   "cc" '(compile :wk "Compile")
  ;;   "cC" '(recompile :wk "Recompile")
  ;;   "cd" '(go-guru-definition :wk "Jump to definition")
  ;;   "cD" '(go-guru-referrers :wk "Jump to references")
  ;;   "ck" '(godoc-at-point :wk "Jump to documentation")
  ;;   )
  )

(use-package gorepl-mode
  :straight t
  :after go-mode
  :commands gorepl-run-load-current-file)

;; (use-package flycheck-golangci-lint
;;     :straight t
;;     :after go-mode
;;     :hook (go-mode . flycheck-golangci-lint-setup)
;;     :custom
;;     (flycheck-golangci-lint-enable-all t)
;;     (flycheck-golangci-lint-fast t)
;;     (flycheck-golangci-lint-config
;;      (expand-file-name "golangci.yml" "~/.config/golangci-lint"))
;;     (flycheck-golangci-lint-tests t))

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

;; (use-package go-guru
;;   :straight t
;;   :after go-mode
;;   :ensure t
;;   :commands go-guru-hl-identifier-mode
;;   :init
;;   (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(provide 'init-go)
;;; init-go.el ends here
