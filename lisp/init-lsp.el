;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :commands lsp
    :hook (((java-mode
             python-mode
	     lua-mode
	     go-mode
	     scala-mode
	     js-mode
             js2-mode
             typescript-mode
             web-mode) . lsp)
	   ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration)
	   )
    :custom
    (lsp-auto-guess-root nil)
    (lsp-prefer-flymake nil) ;; Use flycheck instead of flymake
    (lsp-file-watch-threshold 2000)
    :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
    :config
    (progn
      ;; 注册emmy-lua-lsp
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection (list
                                                               "/usr/bin/java"
                                                               "-cp"
                                                               (expand-file-name  "bin/EmmyLua-LS-all.jar" poly-local-dir)
                                                               "com.tang.vscode.MainKt"))
			:major-modes '(lua-mode)
			:server-id 'emmy-lua
			:priority 2
			:notification-handlers
			(lsp-ht
			 ("emmy/progressReport" 'ignore))
			))

      ;; (make-lsp-client :new-connection (lsp-stdio-connection
      ;; 					(list (expand-file-name "workspace/lua-language-server/bin/macOS/lua-language-server" "~")
      ;;                                         "-E"
      ;;                                         (expand-file-name "workspace/lua-language-server/main.lua" "~")))
      ;;                  :major-modes '(lua-mode)
      ;; 		       :priority 2
      ;;                  :server-id 'lua-language-server)
      ;; (setq lsp-eldoc-hook '(lsp-document-highlight))

      ;; cancel warning
      (advice-add 'lsp-warn
		  :around (lambda (orig-func &rest r)
                            (message (apply #'format-message r))))
      ))

(use-package lsp-ui
    :straight t
    :after lsp-mode
    :diminish
    :commands lsp-ui-mode
    ;; :custom-face
    ;; (lsp-ui-doc-background ((t (:background nil))))
    ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
    :hook (lsp-mode-hook . lisp-ui-mode)
    :general
    (nmap
     :keymaps 'lsp-ui-mode-map
     [remap evil-goto-definition] #'lsp-ui-peek-find-definitions
     "gD" #'lsp-ui-peek-find-references)
    (general-def
	:keymaps 'lsp-ui-peek-mode-map
      "C-j" 'lsp-ui-peek--select-next
      "C-k" 'lsp-ui-peek--select-prev)
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header nil)
    (lsp-ui-doc-include-signature nil)
    (lsp-enable-snippet nil)
    (lsp-enable-file-watchers t)
    (lsp-file-watch-threshold 10000)
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-border (face-foreground 'default))
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-code-actions nil)
    (lsp-flycheck-live-reporting nil)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-doc-background (doom-color 'base4))
    (lsp-ui-doc-border (doom-color 'fg))
    :config
    ;; Use lsp-ui-doc-webkit only in GUI
    (if IS-GUI
	(setq lsp-ui-doc-use-webkit t))
    ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
    ;; https://github.com/emacs-lsp/lsp-ui/issues/243
    (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
      (setq mode-line-format nil))
    )

(use-package lsp-ivy
    :straight t
    :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
    :straight t
    :after company
    :custom (company-lsp-cache-candidates 'auto)
    :config
    (setq
     syntax-checking-enable-by-default t
     lsp-highlight-symbol-at-point nil
     lsp-enable-codeaction nil
     lsp-log-io nil
     lsp-enable-xref t
     lsp-auto-guess-root t
     lsp-diagnostic-package :flymake
     lsp-enable-indentation t
     lsp-enable-completion-at-point t
     lsp-enable-eldoc nil
     lsp-response-timeout 1000
     lsp-file-watch-threshold 150000
     )

    (add-to-list 'lsp-file-watch-ignored "build")


    (setq-default company-frontends
                  '(;; company-tng-frontend
                    company-pseudo-tooltip-frontend
                    ;; company-preview-frontend
                    company-echo-metadata-frontend))
    )

(provide 'init-lsp)
;;; init-lsp ends here
