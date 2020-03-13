;;; lisp/init-go.el -*- lexical-binding: t; -*-

(use-package go-mode
    :straight t
    :ensure t
    :commands (godoc gofmt gofmt-before-save)
    :config
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)

     ;; (general-define-key
     ;;         :prefix "SPC"
     ;;         :non-normal-prefix "M-m"
     ;;         ;;;
     ;;         "c" '(:ignore t :wk "code")
     ;;         "cd" '(go-guru-definition :wk "Jump to definition")
     ;;         "cD" '(go-guru-referrers :wk "Jump to references")
     ;;         "ck" '(godoc-at-point :wk "Jump to documentation")
     ;; )
    )

(use-package gorepl-mode
    :straight t
    :after go-mode
    :commands gorepl-run-load-current-file)



(use-package flycheck-golangci-lint
  :straight t
  :hook (go-mode . flycheck-golangci-lint-setup)
  ;; :after go
  :config
  (setenv "GO111MODULE" "on")
  (setq flycheck-golangci-lint-enable-all t)
  ;; (setq flycheck-golangci-lint-config (concat (expand-file-name "~") "/.config/golangci-lint/golangci.yml"))
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t))


; (use-package flycheck-golangci-lint
;   :straight t
;   :hook (go-mode . flycheck-golangci-lint-setup))

; (use-package company-go
;     :straight t
;   :after go-mode
;   :config
;   (setq company-go-show-annotation t))

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


(general-define-key
 :keymaps 'go-mode-map
 :states '(normal motion visual)
 :prefix "SPC"
 :global-prefix "C-SPC"
             "c" '(:ignore t :wk "code")
             "cc" '(compile :wk "Compile")
             "cC" '(recompile :wk "Recompile")
             "cd" '(go-guru-definition :wk "Jump to definition")
             "cD" '(go-guru-referrers :wk "Jump to references")
             "ck" '(godoc-at-point :wk "Jump to documentation")
 )

(provide 'init-go)
;;; init-go.el ends here
