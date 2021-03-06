;;; lisp/init-eglot.el -*- lexical-binding: t; -*-

;; https://github.com/DEbling/dotfiles/blob/9dc0e347267dd68111baf8e7ab7d33c2e39ed404/.emacs.d/elisp/lang-java.el
;; (defconst jdt-jar-path "~/.emacs.d/.local/jar/org.eclipse.equinox.launcher.jar")
(defconst jdt-jar-path "/opt/jdt-language-server/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar")
(defconst jdt-extra-jvm-args '("-noverify"
			       "-javaagent:/Users/jiya/workspace/dotemacs.d/.local/jar/lombok.jar"
			       ;; "-javaagent:[~/.emacs.d/.local/jar/lombok.jar][classes=META-INF/]"
			       "-Xbootclasspath/a:~/.config/emacs/.local/jar/lombok.jar"
			       "--add-modules=ALL-SYSTEM"
			       "--add-opens"
			       "java.base/java.util=ALL-UNNAMED"
			       "--add-opens"
			       "java.base/java.lang=ALL-UNNAMED"
			       ;; "-configuration"
			       ;; "/opt/jdt-language-server/config_mac"
			       ))

(defun my-eclipse-jdt-contact (interactive)
  "Contact with the jdt server.
If INTERACTIVE, prompt user for details."
  (let* ((cp (getenv "CLASSPATH"))
	 (contact (unwind-protect (progn
				    (setenv "CLASSPATH" jdt-jar-path)
				    (eglot--eclipse-jdt-contact interactive))
		    (setenv "CLASSPATH" cp)))
	 (jdt-class (car contact))
	 (args (cddr contact)))
    (append (list jdt-class "/usr/local/opt/java11/bin/java")
	    jdt-extra-jvm-args args)))
;; (setq aaabb (my-eclipse-jdt-contact))

(defun dart-lsp-contact (interactive)
  (list (executable-find "dart")
        (concat (file-name-directory (nix-executable-find nil "dart"))
                "snapshots/analysis_server.dart.snapshot")
        "--lsp"
        "--client-id=emacs.eglot"))

(use-package eglot
  :straight t
  :hook ((go-mode lua-mode python-mode c-mode c++-mode python-mode java-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake imenu eldoc company))  ;; eglot reinits backends
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
  :functions eglot--eclipse-jdt-contact
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
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

  (add-to-list 'eglot-server-programs
	       '(java-mode .  my-eclipse-jdt-contact))

  (add-to-list 'eglot-server-programs
	       '(dart-mode . dart-lsp-contact))

  (when (executable-find "ccls")
    (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"
 					  "-init={\"compilationDatabaseDirectory\":\"build\"}")))
  (when (executable-find "gopls")
    (add-to-list 'eglot-server-programs `(go-mode . ("/usr/local/bin/gopls" "-logfile=/tmp/gopls.log" "-rpc.trace" "-vv" "--debug=localhost:6060"))))

  (add-hook 'eglot-managed-mode-hook (lambda()
				       (make-local-variable 'company-backends)
				       ;; (setq-local company-backends nil)
				       ;; (setq company-backends
				       ;; 	     '(company-capf
				       ;; 	       ;; company-dabbrev-code
				       ;; 	       (company-files          ; files & directory
				       ;; 		company-keywords       ; keywords
				       ;; 		)
				       ;; 	       (company-abbrev company-dabbrev)))
				       (setq-local company-backends
						   ;; '((company-tabnine :with company-capf :separate)
						   '(company-tabnine
						     company-dabbrev-code
						     (company-files          ; files & directory
						      company-keywords       ; keywords
						      )
						     (company-abbrev company-dabbrev)))
				       ))
  ;; :general
  ;; (leader-def
  ;;   "cC" '(eglot :wk "connect to lsp"))
  ;; (leader-def :keymaps 'eglot-mode-map
  ;;   "ce" '(:ignore t :wk "eglot")
  ;;   "ced" '(xref-find-definitions :wk "Jump to definition")
  ;;   "ceD" '(xref-find-references :wk "Jump to references")'
  ;;   "ce=" #'eglot-format-buffer
  ;;   "cef" #'eglot-format
  ;;   "ceh" #'eglot-help-at-point
  ;;   "cem" #'eglot-events-buffer
  ;;   "cer" #'eglot-rename
  ;;   "ceR" #'eglot-reconnect)
  )

(provide 'init-eglot)
;;; init-eglot.el ends here
