;;; lisp/init-eglot.el -*- lexical-binding: t; -*-

(use-package eglot
  :straight t
  :hook
  ((lua-mode python-mode c-mode c++-mode python-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake))
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 40)
  (eglot-send-changes-idle-time 0.5)
  ;; (eglot-events-buffer-size 500000)
  (eglot-events-buffer-size 0)
  ;; disable symbol highlighting and documentation on hover
  (eglot-ignored-server-capabilites
   '(:documentHighlightProvider
     :signatureHelpProvider
     :hoverProvider))
  ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
  ;; its popup rule causes eglot to steal focus too often.
  (eglot-auto-display-help-buffer nil)
  :config
  (setq eglot-workspace-configuration
        '((:gopls . (:usePlaceholders t
				      :completeUnimported  t
				      :experimentalWorkspaceModule t
				      ;; :experimentalDiagnosticsDelay "800ms"
				      ))))
  ;; emmylua
  (let ((emmylua-jar-path (expand-file-name "bin/EmmyLua-LS-all.jar" poly-local-dir)))
    (add-to-list 'eglot-server-programs
		 `(lua-mode  . ("/usr/bin/java" "-cp" ,emmylua-jar-path "com.tang.vscode.MainKt"))))

  (when (executable-find "ccls")
    (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"
 					  "-init={\"compilationDatabaseDirectory\":\"build\"}")))
  (when (executable-find "gopls")
    (add-to-list 'eglot-server-programs `(go-mode . ("gopls" "-logfile=/tmp/gopls.log" "-rpc.trace" "-vv" "--debug=localhost:6060"))))

  (add-hook 'eglot-managed-mode-hook (lambda()
				       (make-local-variable 'company-backends)
				       (setq company-backends nil)
				       ;; (setq company-backends
				       ;; 	     '(company-capf
				       ;; 	       ;; company-dabbrev-code
				       ;; 	       (company-files          ; files & directory
				       ;; 		company-keywords       ; keywords
				       ;; 		)
				       ;; 	       (company-abbrev company-dabbrev)))
				       (setq company-backends
					     '((company-tabnine :with company-capf :separate)
					       company-dabbrev-code
					       (company-files          ; files & directory
						company-keywords       ; keywords
						)
					       (company-abbrev company-dabbrev)))
				       )))

;; fix (void-function project-root)
(defun project-root (project)
  (car (project-roots project)))

(provide 'init-eglot)
;;; init-eglot.el ends here
