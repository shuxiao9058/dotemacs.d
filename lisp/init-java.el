;;; lisp/init-java.el -*- lexical-binding: t; -*-

;; LSPJavaPac
(use-package lsp-java
  :straight t
  :after lsp-mode
  ;; :if *mvn*
  ;; :config
  ;; (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name ".config/emacs/.local/jar/jdt-language-server/" user-emacs-directory))
  (lsp-java-workspace-dir (expand-file-name ".config/emacs/.local/jar/workspace/" user-emacs-directory))
  (lsp-java-workspace-cache-dir (expand-file-name "ws-cache" poly-cache-dir))
  ;; :pretty-hydra (hydra-lsp-java (:color blue :title "LSP Java" :quit-key "q")
  ;;                               ("Imports"
  ;;                                (("i o" lsp-java-organize-imports "organize")
  ;;                                 ("i a" lsp-java-add-import "add"))
  ;;                                "Project"
  ;;                                (("p b" lsp-java-build-project "build")
  ;;                                 ("p u" lsp-java-update-project-configuration "update config"))
  ;;                                "Update"
  ;;                                (("u u" lsp-java-update-user-settings "user settings")
  ;;                                 ("u s" lsp-java-update-server "server"))
  ;;                                "Generate"
  ;;                                (("C-g s" lsp-java-generate-to-string "to string")
  ;;                                 ("C-g h" lsp-java-generate-equals-and-hash-code "equals and hash code")
  ;;                                 ("C-g o" lsp-java-generate-overrides "overrides")
  ;;                                 ("C-g g" lsp-java-generate-getters-and-setters "getters and setters"))
  ;;                                "Extract"
  ;;                                (("e c" lsp-java-extract-to-constant "to constant")
  ;;                                 ("e m" lsp-java-extract-method "method"))
  ;;                                "Create"
  ;;                                (("c p" lsp-java-create-parameter "parameter")
  ;;                                 ("c f" lsp-java-create-field "field")
  ;;                                 ("c l" lsp-java-create-local "local"))
  ;;                                "Other"
  ;;                                (("m" lsp-java-add-unimplemented-methods "add unimplemented methods")
  ;;                                 ("n" lsp-java-actionable-notifications "actionable notifications")))
  :config
  ;; (require 'lsp-java-boot)
  (require 'dap-java)
  ;; (add-hook 'java-mode-local-vars-hook #'lsp-java-boot-lens-mode)

  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4
                                    tab-width 4
                                    indent-tabs-mode t)))

  (setq kei/lombok-jar (expand-file-name "lombok.jar" "~/workspace/dotemacs.d/.local/jar" ))
  (setq lsp-java-vmargs
        `("-noverify"
          "-Xmx2G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          ,(concat "-javaagent:" kei/lombok-jar)
          ,(concat "-Xbootclasspath/a:" kei/lombok-jar)
	  "--add-modules=ALL-SYSTEM"
	  "--add-opens"
	  "java.base/java.util=ALL-UNNAMED"
	  ;; "--add-opens"
	  ;; "java.base/java.lang=ALL-UNNAMED"
	  ;; "-configuration"
	  ;; "/opt/jdt-language-server/config_mac"
	  )
	)
  )

;;;###autoload
(defun +java|android-mode-maybe ()
  "Enable `android-mode' if this looks like an android project.
It determines this by the existence of AndroidManifest.xml or
src/main/AndroidManifest.xml."
  (when (f-exists? (or "AndroidManifest.xml"
                       "src/main/AndroidManifest.xml"))
    (android-mode +1)))

(use-package android-mode
  :straight t
  :commands android-mode
  :init
  (add-hook 'java-mode #'+java|android-mode-maybe)
  (add-hook 'groovy-mode #'+java|android-mode-maybe)
  (add-hook 'nxml-mode #'+java|android-mode-maybe))


(use-package groovy-mode
  :straight t
  :mode "\\.g\\(?:radle\\|roovy\\)$")


;; (use-package dap-java
;;   :straight nil
;;   :after (lsp-java)
;;   )

(provide 'init-java)
;;; init-java.el ends here
