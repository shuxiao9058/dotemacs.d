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
	   c-mode
	   cc-mode
	   c++-mode
	   C/*l-mode
	   web-mode) . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-after-open . (lambda()
			     (make-local-variable 'company-backends)
			     (setq company-backends nil)
			     (setq company-backends
				   '((company-tabnine :with company-capf :separate)
				     company-dabbrev-code
				     (company-files          ; files & directory
				      company-keywords       ; keywords
				      )
				     (company-abbrev company-dabbrev)))))
	 )
  :custom
  (lsp-idle-delay 0.5)                 ;; lazy refresh
  (lsp-log-io nil)
  (lsp-enable-xref t)
  (lsp-enable-indentation t)
  (lsp-enable-completion-at-point t)
  (lsp-response-timeout 1000)
  (lsp-enable-folding nil)             ;; use `evil-matchit' instead
  (lsp-diagnostic-package :none)   ;; prefer flycheck disable
  (lsp-flycheck-live-reporting nil)    ;; obey `flycheck-check-syntax-automatically'
  (lsp-prefer-capf t)                  ;; using `company-capf' by default
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-file-watchers nil)       ;; turn off for better performance
  ;; (lsp-file-watch-threshold 10000)
  (lsp-enable-text-document-color nil) ;; as above
  (lsp-enable-symbol-highlighting nil) ;; as above
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc displays in minibuffer
  (lsp-enable-imenu nil)
  ;; (lsp-clients-emmy-lua-jar-path (expand-file-name  "bin/EmmyLua-LS-all.jar" poly-local-dir))
  :config
  ;; ;; lsp-lua
  ;; ;; 暂时还有点问题，先不用了
  (require 'init-lsp-lua)
  ;; cancel warning
  (advice-add 'lsp-warn
	      :around (lambda (orig-func &rest r)
			(message (apply #'format-message r))))

  ;; (add-hook 'lsp-after-open-hook
  ;; 	       (lambda()
  ;; 		 (setq company-backends (delete 'company-capf company-backends))))
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

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
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

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp ends here
