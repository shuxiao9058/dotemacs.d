;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

(defvar my-disable-lsp-completion nil
  "If non-nil, disable lsp-completion-enable, can work with .dir-locals
   ((nil . ((eval . (setq-local my-disable-lsp-completion t)))))
.")

;; (defun lsp-configure-buffer@after()
;;   "disable lsp-completion-enable"
;;   (progn
;;     (when my-disable-lsp-completion
;;       (setq-local lsp-completion-enable nil
;; 		  lsp-modeline-code-actions-enable nil
;; 		  ))))

;; ;; disable lsp-completion-enable with some project
;; (advice-add 'lsp-configure-buffer :after 'lsp-configure-buffer@after)

(defun my/local-variables-hook()
  "disable lsp-completion-enable"
  (when (bound-and-true-p my-disable-lsp-completion)
    (setq-local lsp-completion-enable nil
		lsp-modeline-code-actions-enable nil)
    )
  (when (derived-mode-p 'go-mode)
    (lsp-deferred)))

(defun my/lsp-go()
  "run after local variables loaded"
  ;; (setq lsp-gopls-build-flags '("-mod=readonly"))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\vendor$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].git$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]internal$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.gocache$")
  ;; https://github.com/golang/tools/commit/b2d8b0336
  (setq-local lsp-completion-filter-on-incomplete nil)
  (add-hook 'hack-local-variables-hook #'my/local-variables-hook))

;; ;; lsp-diagnostics-updated-hook
;; (defun my/auto-restart-lsp-after-diagnostics ()
;;   "After diagnostics handler."
;;   (save-excursion
;;     (condition-case _err
;;         ;; (with-current-buffer (get-buffer-create lsp-treemacs-errors-buffer-name)
;;         ;;   ;; (treemacs-update-node '(:custom LSP-Errors) t)
;;         ;;   )
;; 	(error)))
;;   )

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

  (advice-add 'lsp--window-log-message :after 'my/lsp--window-log-message@after)
  ;; )
  )


;; (defun my/lsp-notify-wrapper (params)
;;   "filter lsp notify message, then try restart lsp"
;;   ;; (let ((lsp--virtual-buffer-mappings (ht)))
;;   (pcase (plist-get params :method)
;;     (`"window/logMessage"
;;      ;; (setq my/params params)
;;      (-let [(&plist :params
;;                     (&plist :type
;;                             :message))
;;             params]
;;        (when (string-match-p (regexp-quote "goplsworkspace requires") message)
;; 	 (message "receive goplsworkspace requires error, restart lsp")
;; 	 (lsp-workspace-restart)
;; 	 )
;;        ))
;;     )
;;   ;; )
;;   )


(use-package lsp-mode
  :straight t
  :diminish
  :commands (lsp lsp-deferred lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports)
  :hook (((java-mode
	   python-mode
	   lua-mode
	   ;; go-mode
	   scala-mode
	   js-mode
	   js2-mode
	   typescript-mode
	   ;; lisp-mode
	   ;; emacs-lisp-mode
	   ;; c-mode
	   ;; cc-mode
	   ;; c++-mode
	   ;; C/*l-mode
	   web-mode) . lsp-deferred)
	 (go-mode . my/lsp-go) ;; disable lsp-completion-enable with go, since gopls is quite slow
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
  (lsp-gopls-server-path "/usr/local/bin/gopls")
  (lsp-gopls-server-args '("-debug" "127.0.0.1:3000" "-logfile=/tmp/gopls-emacs.log" ;; "-rpc.trace" "-vv"
			   ))
  :config

  ;; Run ros install cxxxr/cl-lsp
  ;; also see repo https://github.com/cxxxr/cl-lsp.git
  ;; (add-to-list 'lsp-language-id-configuration '(lisp-mode "lisp"))
  ;; (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode "elisp"))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "/usr/local/bin/cl-lsp")
  ;;                   :major-modes '(lisp-mode emacs-lisp-mode)
  ;;                   :server-id 'cl-lsp))

  (lsp-register-custom-settings
   `(;; ("gopls.experimentalWorkspaceModule" t t)
     ("gopls.allExperiments" nil nil)
     ;; ("gopls.experimentalPackageCacheKey" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.deepCompletion" nil nil)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" ,(if (executable-find "staticcheck") t nil) t)
     ("gopls.completionBudget" "200ms" nil)
     ("gopls.semanticTokens" nil nil)
     ;; ("gopls.allExperiments" t t)
     ("gopls.matcher" "Fuzzy" nil)
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
     ("gopls.buildFlags" ["-mod=readonly"])
     ("gopls.env" lsp-go-env)
     ("gopls.linkTarget" lsp-go-link-target)
     ("gopls.gofumpt" ,(if (executable-find "gofumpt") t nil) t)
     ("gopls.directoryFilters" ["-vendor" "-internal" "-.gocache" "-.git" "-!out"])
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

(use-package dap-mode
  :straight t
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  ;; (dap-mode t)
  (dap-ui-mode t)
  )

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list
  )

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :diminish
  ;; :custom-face
  ;; (lsp-ui-doc-background ((t (:background nil))))
  ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :hook (lsp-mode-hook . lisp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature nil)
  ;; (lsp-ui-doc-position 'top)
  ;; (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-diagnostics nil)
  :config
  ;; ;; Use lsp-ui-doc-webkit only in GUI
  ;; (if IS-GUI
  ;; 	(setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  :general
  (nvmap :keymaps '(lsp-ui-mode-map)
    [remap evil-goto-definition] #'lsp-ui-peek-find-definitions
    "gD" #'lsp-ui-peek-find-references)
  (:keymaps '(lsp-ui-peek-mode-map)
	    "C-j" 'lsp-ui-peek--select-next
	    "C-k" 'lsp-ui-peek--select-prev)
  )

;; (use-package lsp-ivy
;;   :straight t
;;   :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp ends here
