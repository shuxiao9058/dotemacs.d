;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :diminish
  :commands (lsp lsp-deferred lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports)
  :hook (((java-mode
	   python-mode
	   lua-mode
	   go-mode
	   scala-mode
	   js-mode
	   js2-mode
	   typescript-mode
	   ;; c-mode
	   ;; cc-mode
	   ;; c++-mode
	   ;; C/*l-mode
	   web-mode) . lsp-deferred)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)

	 ;; (lsp-after-open . (lambda()
	 ;; 		     (make-local-variable 'company-backends)
	 ;; 		     (setq company-backends nil)
	 ;; 		     (setq company-backends
	 ;; 			   '(company-capf
	 ;; 			     company-dabbrev-code
	 ;; 			     (company-files          ; files & directory
	 ;; 			      company-keywords       ; keywords
	 ;; 			      )
	 ;; 			     (company-abbrev company-dabbrev)))))
	 ;; (lsp-after-open . (lambda()
	 ;; 		     (make-local-variable 'company-backends)
	 ;; 		     ;; (setq company-backends nil)
	 ;; 		     (setq-local company-backends
	 ;; 				 '(
	 ;; 				   ;; company-tabnine
	 ;; 				   ;; company-capf
	 ;; 				   (company-capf :with
	 ;; 						 company-tabnine    :separate)
	 ;; 				   company-dabbrev-code
	 ;; 				   (company-files          ; files & directory
	 ;; 				    company-keywords       ; keywords
	 ;; 				    )
	 ;; 				   (company-abbrev company-dabbrev)))))
	 )
  :custom
  (lsp-restart 'ignore)
  (lsp-server-trace nil)
  (lsp-auto-configure t)
  (lsp-idle-delay 0.1)                 ;; lazy refresh
  (lsp-log-io nil)
  (lsp-print-performance nil)
  (lsp-auto-execute-action nil) ;; Auto-execute single action
  (lsp-document-sync-method nil) ;; use default method recommended by server. 'incremental 'full
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-diagnostics-provider :auto)
  (lsp-diagnostic-clean-after-change t)
  (lsp-enable-indentation nil)
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-response-timeout 2)
  (lsp-enable-folding nil)             ;; use `evil-matchit' instead
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
  ;; (lsp-signature-auto-activate nil)
  (lsp-signature-auto-activate #'(:after-completion :on-trigger-char)) ; nil
  (lsp-signature-render-documentation nil "Display signature documentation in `eldoc'")
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc displays in minibuffer
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet t)
  (lsp-enable-imenu nil)
  (lsp-enable-links nil) ;;
  (lsp-prefer-flymake nil) ;; Use lsp-ui and flycheck
  (lsp-imenu-container-name-separator "⦿")
  (lsp-imenu-show-container-name t)
  ;; (flymake-fringe-indicator-position 'right-fringe)
  ;; (lsp-clients-emmy-lua-jar-path (expand-file-name  "bin/EmmyLua-LS-all.jar" poly-local-dir))
  (lsp-gopls-server-path "/usr/local/gopath/bin/gopls")
  (lsp-gopls-server-args '("-debug" "127.0.0.1:3000" "-logfile=/tmp/gopls-emacs.log" "-rpc.trace" "-vv" ))
  :config
  (lsp-register-custom-settings
   `(("gopls.experimentalWorkspaceModule" t t)
     ("gopls.experimentalPackageCacheKey" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" ,(if (executable-find "staticcheck") t nil) t)
     ("gopls.completionBudget" "200ms" nil)
     ("gopls.semanticTokens" t t)
     ("gopls.allExperiments" t t)
     ("gopls.matcher" "Fuzzy" nil)
     ("gopls.hoverKind" lsp-go-hover-kind)
     ("gopls.codelenses" lsp-go-codelenses)
     ;; ("gopls.analyses.unusedparams" nil nil)
     ;; ST1003 CamelCase
     ;; ST1021 comment on exported type
     ;; ST1016 methods on the same type should have the same receiver name
     ;; ST1020 comment on exported function
     ("gopls.analyses" ,(mapcar (lambda (a) (cons a :json-false)) '(unusedparams composites ST1003  ST1021 ST1016 SA5011 ST1020)))
     ("gopls.buildFlags" lsp-go-build-flags)
     ("gopls.env" lsp-go-env)
     ("gopls.linkTarget" lsp-go-link-target)
     ("gopls.gofumpt" ,(if (executable-find "gofumpt") t nil) t)
     ;; ("gopls.directoryFilters" (quote ("-" "+vendor" "+internal")))
     ))

  ;; ;; lsp-lua
  ;; ;; 暂时还有点问题，先不用了
  (require 'init-lsp-lua)
  ;; cancel warning
  (advice-add 'lsp-warn
	      :around (lambda (orig-func &rest r)
			(message (apply #'format-message r))))
  :general
  (leader-def :keymaps '(lsp-mode-map)
    "c" '(:ignore t :wk "code")
    "cc" '(compile :wk "Compile")
    "cC" '(recompile :wk "Recompile")
    "cd" '(lsp-find-definition :wk "Jump to definition")
    "cr" '(lsp-rename :wk "lsp rename")
    "cD" '(lsp-find-reference :wk "Jump to references")
    "ck" '(lsp-find-document :wk "Jump to documentation")
    )
  )

;; (use-package lsp-ui
;;   :straight t
;; 					; :after lsp-mode
;;   :diminish
;; 					; :commands lsp-ui-mode
;;   ;; :custom-face
;;   ;; (lsp-ui-doc-background ((t (:background nil))))
;;   ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :hook (lsp-mode-hook . lisp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-enable nil)
;;   (lsp-ui-doc-header nil)
;;   (lsp-ui-doc-include-signature nil)
;;   ;; (lsp-ui-doc-position 'top)
;;   ;; (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   (lsp-ui-sideline-show-diagnostics nil)
;;   :config
;;   ;; ;; Use lsp-ui-doc-webkit only in GUI
;;   ;; (if IS-GUI
;;   ;; 	(setq lsp-ui-doc-use-webkit t))
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil))
;;   :general
;;   (nvmap :keymaps '(lsp-ui-mode-map)
;;     [remap evil-goto-definition] #'lsp-ui-peek-find-definitions
;;     "gD" #'lsp-ui-peek-find-references)
;;   (:keymaps '(lsp-ui-peek-mode-map)
;; 	    "C-j" 'lsp-ui-peek--select-next
;; 	    "C-k" 'lsp-ui-peek--select-prev)
;;   )

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp ends here
