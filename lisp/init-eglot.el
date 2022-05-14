;;; lisp/init-eglot.el -*- lexical-binding: t; -*-

;; https://github.com/DEbling/dotfiles/blob/9dc0e347267dd68111baf8e7ab7d33c2e39ed404/.emacs.d/elisp/lang-java.el
;; (defconst jdt-jar-path "~/.emacs.d/.local/jar/org.eclipse.equinox.launcher.jar")
;; (defconst jdt-jar-path "/opt/jdt-language-server/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar")
(defconst jdt-jar-path (expand-file-name "jdt-language-server/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar" "~/workspace"))
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
    (append (list jdt-class "/usr/bin/java")
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
  :hook ((go-mode lua-mode beancount-mode python-mode c-mode c++-mode python-mode java-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake imenu eldoc))  ;; eglot reinits backends
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 40)
  (eglot-send-changes-idle-time 0.5)
  (eglot-confirm-server-initiated-edits nil)
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
        '((:gopls .
		  (:usePlaceholders t
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
	       `(beancount-mode .  ("beancount-language-server")))

  (add-to-list 'eglot-server-programs
	       '(dart-mode . dart-lsp-contact))

  (when (executable-find "ccls")
    (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"
 					  "-init={\"compilationDatabaseDirectory\":\"build\"}")))
  (when (executable-find "gopls")
    (add-to-list 'eglot-server-programs '(go-mode . ("gopls"))))

  ;; (tabnine-completion-at-point cape-line cape-symbol cape-ispell cape-dabbrev cape-tex cape-file tags-completion-at-point-function)
  ;;   completion-at-point-functions is a variable defined in ‘minibuffer.el’.

  ;; Its value is (eglot-completion-at-point t)
  ;; Local in buffer robot.go; global value is
  ;; (tabnine-completion-at-point cape-line cape-symbol cape-ispell cape-dabbrev cape-tex cape-file tags-completion-at-point-function)

  (add-hook 'eglot-managed-mode-hook
	    (lambda()
	      (progn
                (lsp/non-greedy-eglot)
                (lsp/extra-capf))
	      ;; (make-local-variable 'completion-at-point-functions)
	      ;; (setq-local completion-at-point-functions
	      ;; 		  '(tabnine-completion-at-point cape-line cape-symbol cape-ispell cape-dabbrev cape-tex cape-file tags-completion-at-point-function))
	      ))

  ;; (add-hook 'eglot-managed-mode-hook
  ;; 	    (lambda()
  ;; 	      (make-local-variable 'company-backends)
  ;; 	      (setq-local company-backends
  ;; 			  '(company-tabnine-capf company-capf company-tabnine
  ;; 						 (company-dabbrev company-dabbrev-code)
  ;; 						 company-keywords company-files))))
  ;; (add-hook 'eglot-managed-mode-hook (lambda()
  ;; 				       (make-local-variable 'company-backends)
  ;; 				       ;; (setq-local company-backends nil)
  ;; 				       ;; (setq company-backends
  ;; 				       ;; 	     '(company-capf
  ;; 				       ;; 	       ;; company-dabbrev-code
  ;; 				       ;; 	       (company-files          ; files & directory
  ;; 				       ;; 		company-keywords       ; keywords
  ;; 				       ;; 		)
  ;; 				       ;; 	       (company-abbrev company-dabbrev)))
  ;; 				       (setq-local company-backends
  ;; 						   ;; '((company-tabnine :with company-capf :separate)
  ;; 						   '(company-tabnine
  ;; 						     company-dabbrev-code
  ;; 						     (company-files          ; files & directory
  ;; 						      company-keywords       ; keywords
  ;; 						      )
  ;; 						     (company-abbrev company-dabbrev)))
  ;; 				       ))
  ;; )
  :bind (:map eglot-mode-map
	      ("C-c C-r" . eglot-rename)
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer))
  )


(defun lsp/non-greedy-eglot ()
  "Making Eglot capf non-greedy."
  (progn
    (fset 'non-greedy-eglot
          (cape-capf-buster
           (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)))
    (setq completion-at-point-functions
          (list #'non-greedy-eglot))))

(defun lsp/extra-capf ()
  "Adding extra capf during LSP startup."
  (let ((tmp-symbol (intern (concat "capf/" (symbol-name major-mode)))))
    (unless (null (symbol-function tmp-symbol))
      (funcall (symbol-function tmp-symbol)))))

(provide 'init-eglot)
;;; init-eglot.el ends here
