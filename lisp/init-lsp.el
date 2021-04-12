;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

(defvar my-disable-lsp-completion nil
  "If non-nil, disable lsp-completion-enable, can work with .dir-locals
   ((nil . ((eval . (setq-local my-disable-lsp-completion t)))))
.")

(defun my/local-variables-hook()
  "disable lsp-completion-enable"
  (when (bound-and-true-p my-disable-lsp-completion)
    (setq-local lsp-completion-enable nil
		;; lsp-modeline-code-actions-enable nil
		))
  (when (derived-mode-p 'go-mode
			'java-mode
			'python-mode
			'lua-mode
			'scala-mode
			'js-mode
			'js2-mode
			'typescript-mode
			'c-mode
			'c++-mode
			'clojure-mode)
    ;; https://github.com/golang/tools/commit/b2d8b0336
    (setq-local lsp-completion-filter-on-incomplete nil)
    (lsp-deferred)))

(with-eval-after-load 'lsp-mode
  (lsp-defun my/lsp--window-log-message@after(workspace (&ShowMessageRequestParams :message :type))
    "filter lsp log message, then try restart lsp when neeeded"
    ;; (ignore
    ;; (print (format "receive goplsworkspace requires error, restart lsp, message: %s" message))
    (when (string-match-p (regexp-quote "goplsworkspace requires") message)
      (print (format "receive gopls workspace requires error, restart lsp, log message: %s" message))
      ;; (message "receive goplsworkspace requires error, restart lsp")
      (lsp-workspace-restart workspace)
      ))

  (advice-add 'lsp--window-log-message :after 'my/lsp--window-log-message@after))

(use-package lsp-mode
  :straight t
  :diminish
  :commands (lsp lsp-deferred lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-restart 'auto-restart)
  ;; (lsp-restart 'ignore)
  (lsp-server-trace nil)
  (lsp-auto-configure t)
  (lsp-idle-delay 0.1)                 ;; lazy refresh
  (lsp-log-io nil)
  (lsp-log-max nil)
  (lsp-print-performance nil)
  (lsp-auto-execute-action t) ;; Auto-execute single action
  (lsp-document-sync-method nil) ;; use default method recommended by server. 'incremental 'full
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-segments '(count name icon))
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-diagnostics-provider :none)
  (lsp-diagnostic-clean-after-change nil)
  (lsp-enable-indentation nil)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-response-timeout 5)
  (lsp-tcp-connection-timeout 2)
  (lsp-enable-folding t)             ;; use `evil-matchit' instead
  (lsp-diagnostic-package :none)   ;; prefer flycheck disable
  (lsp-modeline-diagnostics-enable nil)
  (lsp-diagnostics-disabled-modes '(js-mode go-mode))
  (lsp-flycheck-live-reporting nil)    ;; obey `flycheck-check-syntax-automatically'
  (lsp-completion-provider :none)    ;; set company-backends manually
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-file-watchers nil)       ;; turn off for better performance
  ;; (lsp-file-watch-threshold 10000)
  (lsp-enable-text-document-color nil) ;; as above
  (lsp-enable-symbol-highlighting nil) ;; as above
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  ;; (lsp-signature-auto-activate #'(:after-completion :on-trigger-char)) ; nil
  (lsp-signature-auto-activate nil) ; nil
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc displays in minibuffer
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet nil)
  (lsp-enable-imenu nil)
  (lsp-enable-links nil) ;;
  (lsp-prefer-flymake nil) ;; Use lsp-ui and flycheck
  (lsp-imenu-container-name-separator "⦿")
  (lsp-imenu-show-container-name nil)
  ;; (flymake-fringe-indicator-position 'right-fringe)
  ;; (lsp-clients-emmy-lua-jar-path (expand-file-name  "bin/EmmyLua-LS-all.jar" poly-local-dir))
  (lsp-clients-emmy-lua-jar-path (expand-file-name "workspace/EmmyLua-LanguageServer/EmmyLua-LS/build/libs/EmmyLua-LS-all.jar" "~"))
  (lsp-gopls-server-path "/usr/local/bin/gopls")
  (lsp-gopls-server-args '("-debug" "127.0.0.1:3000" "-logfile=/tmp/gopls-emacs.log" ;; "-rpc.trace" "-vv"
			   ))
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\vendor$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].git$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]internal$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.gocache$")

  (add-hook 'hack-local-variables-hook #'my/local-variables-hook)

  (lsp-register-custom-settings
   `(;; ("gopls.experimentalWorkspaceModule" t t)
     ("gopls.allExperiments" t t)
     ;; ("gopls.experimentalPackageCacheKey" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.deepCompletion" nil nil)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" ,(if (executable-find "staticcheck") t nil) t)
     ("gopls.completionBudget" "200ms" nil)
     ("gopls.semanticTokens" nil nil)
     ;; ("gopls.allExperiments" t t)
     ("gopls.matcher" "Fuzzy" t)
     ("gopls.hoverKind" "NoDocumentation" nil)
     ;; ("gopls.codelenses" '((gc_details . t) (generate . t) (regenerate_cgo . t) (test . t) (tidy . t) (upgrade_dependency . t) (vendor . t)))
     ("gopls.codelenses"  nil nil)
     ;; ("gopls.analyses.unusedparams" nil nil)
     ;; ST1003 CamelCase
     ;; ST1021 comment on exported type
     ;; ST1016 methods on the same type should have the same receiver name
     ;; ST1020 comment on exported function
     ;; ST1005 error strings should not be capitalized
     ;; SA9003 empty branch
     ;; ST1022 comment on exported var
     ;; S1023 redundant break statement
     ;; SA4011 ineffective break statement. Did you mean to break out of the outer loop?
     ;; SA4010 this result of append is never used, except maybe in other appends
     ;; S1007 should use raw string (`...`) with regexp.Compile to avoid having to escape twice
     ("gopls.analyses" ,(mapcar (lambda (a) (cons a :json-false))
				'(unusedparams composites ST1003  ST1021 ST1016 SA5011 ST1020 ST1005 SA9003 SA4006 ST1022 S1023 SA4011 SA4010)))
     ;; ("gopls.buildFlags" ["-mod=readonly"])
     ("gopls.env" lsp-go-env)
     ("gopls.linkTarget" lsp-go-link-target)
     ("gopls.gofumpt" ,(if (executable-find "gofumpt") t nil) t)
     ;; ("gopls.directoryFilters" ["-vendor" "-internal" "-.gocache" "-.git" "-!out"])
     ))

  ;; cancel warning
  (advice-add 'lsp-warn
	      :around (lambda (orig-func &rest r)
			(message (apply #'format-message r))))
  )

(use-package dap-mode
  :straight t
  :disabled
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  ;; (dap-mode t)
  (dap-ui-mode t)
  )

(use-package lsp-treemacs
  :straight t
  :disabled
  :commands lsp-treemacs-errors-list
  )

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :disabled
  :diminish
  ;; :custom-face
  ;; (lsp-ui-doc-background ((t (:background nil))))
  ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :hook (lsp . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-max-height 45)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'frame)
  ;; (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-use-childframe nil)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-imenu-window-width 200)
  (lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                         ,(face-foreground 'font-lock-string-face)
                         ,(face-foreground 'font-lock-constant-face)
                         ,(face-foreground 'font-lock-variable-name-face)))
  :config
  ;; ;; Use lsp-ui-doc-webkit only in GUI
  ;; (when IS-GUI
  ;;   (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  )

(provide 'init-lsp)
;;; init-lsp ends here
