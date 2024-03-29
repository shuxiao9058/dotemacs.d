;;; lisp/init-company.el -*- lexical-binding: t; -*-

(use-package company
  :straight t
  :ensure t
  :pdump (company-capf company-files company-keywords company-abbrev company-dabbrev company-dabbrev-code)
  :custom
  (company-minimum-prefix-length 3)
  (company-async-wait 0.5)
  (company-async-timeout 1)
  ;; (company-require-match 'never)
  ;; (company-tooltip-limit           20)
  (company-tooltip-align-annotations t) ;; Align annotation to the right side.
  (company-auto-commit nil)
  (company-selection-wrap-around t)
  (company-begin-commands '(self-insert-command org-self-insert-command))
  (company-require-match nil)
  ;; Don't use company in the following modes
  (company-global-modes '(not eshell-mode shell-mode comint-mode erc-mode gud-mode rcirc-mode
			      minibuffer-inactive-mode))
  (company-candidates-length 10)
  (company-echo-delay 0)
  ;; Trigger completion immediately.
  ;; (company-idle-delay nil)
  (company-idle-delay 0.2)
  (company-tooltip-idle-delay 0.5)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :hook (after-init . global-company-mode)
  :config
  (setq company-frontends '(
			    ;; company-pseudo-tooltip-frontend
			    company-pseudo-tooltip-unless-just-one-frontend
			    ;; company-preview-frontend
			    company-preview-if-just-one-frontend
			    company-echo-metadata-frontend))
  ;; set default `company-backends'
  (setq company-backends
        '(company-tabnine-capf
	  company-capf
	  company-tabnine
          (company-dabbrev company-dabbrev-code)
          company-keywords
          company-files))
  ;; (setq-default company-backends
  ;; 		  '(;; company-tabnine
  ;; 		    ;; (company-tabnine :with company-capf :separate)
  ;; 		    company-capf
  ;; 		    company-dabbrev-code
  ;; 		    (company-files          ; files & directory
  ;; 		     company-keywords       ; keywords
  ;; 		     )
  ;; 		    (company-abbrev company-dabbrev)))
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  (unbind-key (kbd "M-n") company-active-map)
  (unbind-key (kbd "M-p") company-active-map)
  :bind
  (:map company-active-map
	("C-n"   .  company-select-next)
	("C-p"     . company-select-previous)
	("C-j"     . company-select-next)
	("C-k"     . company-select-previous)
	("C-h"     . company-show-doc-buffer)
	("C-u"    . company-previous-page)
	("C-d"     . company-next-page)
	("C-s"    . company-filter-candidates)
	("C-SPC"   . company-complete-common)
	("TAB"     . company-complete-common-or-cycle)
	([tab]     . company-complete-common-or-cycle)
	([backtab] . company-select-previous)
	:map company-search-map
        ("C-n"     . company-select-next-or-abort)
	("C-p"     . company-select-previous-or-abort)
	("C-j"     . company-select-next-or-abort)
	("C-k"    .  company-select-previous-or-abort)
	("C-s"    .  (lambda () (interactive) (company-search-abort) (company-filter-candidates)))
	([escape]  . company-search-abort)))

;; (use-package company-posframe
;;   :straight t
;;   :after company
;;   :if IS-GUI
;;   ;; :if  (and (not (package-installed-p 'company-box))
;;   ;; 	      (>= emacs-major-version 26)
;;   ;; 	      IS-GUI)
;;   :hook (global-company-mode . company-posframe-mode))

;; (use-package company-box
;;   :straight t
;;   ;; :straight (company-box :local-repo "~/workspace/company-box")
;;   :after company
;;   :hook (company-mode . company-box-mode)
;;   :if (and (>= emacs-major-version 26) IS-GUI)
;;   :diminish
;;   :custom
;;   (company-box-enable-icon IS-GUI)
;;   (company-box-doc-enable t)     ; eldoc performance issue
;;   (company-box-max-candidates 50)
;;   (company-box-show-single-candidate t)
;;   (company-box-doc-delay 0.3)
;;   :config
;;   (add-to-list 'company-box-backends-colors '(company-tabnine . "gold4"))
;;   (add-to-list 'company-box-backends-colors '(company-predictive . "darkred"))
;;   (add-to-list 'company-box-backends-colors '(company-dabbrev-code . "maroon"))
;;   (setq company-frontends (delq 'company-pseudo-tooltip-frontend company-frontends))
;;   (with-eval-after-load 'all-the-icons
;;     (defun +company-box-icons--elisp (candidate)
;;       (when (derived-mode-p 'emacs-lisp-mode)
;; 	(let ((sym (intern candidate)))
;; 	  (cond ((fboundp sym) 'ElispFunction)
;; 		((boundp sym) 'ElispVariable)
;; 		((featurep sym) 'ElispFeature)
;; 		((facep sym) 'ElispFace)))))

;;     (defun my-company-box-icon (family icon face &rest args)
;;       "Defines icons using `all-the-icons' for `company-box'."
;;       (when icon
;; 	(pcase family
;; 	  ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 :face face args))
;; 	  ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575 :face face))
;; 	  ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 :face face args))
;; 	  ('alltheicon (all-the-icons-alltheicon icon :height 0.8 :face face args)))))

;;     (setq company-box-icons-alist 'company-box-icons-all-the-icons
;; 	  company-box-backends-colors nil
;; 	  company-box-icons-functions
;; 	  '(company-box-icons--yasnippet company-box-icons--lsp +company-box-icons--elisp company-box-icons--acphp)
;; 	  company-box-icons-all-the-icons
;; 	  `((Unknown . ,(my-company-box-icon 'material "find_in_page" 'all-the-icons-purple))
;; 	    (Text . ,(my-company-box-icon 'material "text_fields" 'all-the-icons-green))
;; 	    (Method . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	    (Function . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	    (Constructor . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	    (Field . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	    (Variable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	    (Class . ,(my-company-box-icon 'faicon "cog" 'all-the-icons-orange))
;; 	    (Interface . ,(my-company-box-icon 'faicon "info" 'all-the-icons-orange))
;; 	    (Module . ,(my-company-box-icon 'faicon "cogs" 'all-the-icons-orange))
;; 	    (Property . ,(my-company-box-icon 'material "settings" 'all-the-icons-dyellow))
;; 	    (Unit . ,(my-company-box-icon 'faicon "tag" 'all-the-icons-orange))
;; 	    (Value . ,(my-company-box-icon 'material "filter_none" 'all-the-icons-blue))
;; 	    (Enum . ,(my-company-box-icon 'faicon "list-ul" 'all-the-icons-lcyan))
;; 	    (Keyword . ,(my-company-box-icon 'material "filter_center_focus" 'all-the-icons-red))
;; 	    (Snippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	    (Color . ,(my-company-box-icon 'material "color_lens" 'all-the-icons-pink))
;; 	    (File . ,(my-company-box-icon 'material "insert_drive_file" 'all-the-icons-dsilver))
;; 	    (Reference . ,(my-company-box-icon 'material "collections_bookmark" 'all-the-icons-red))
;; 	    (Folder . ,(my-company-box-icon 'material "folder_open" 'all-the-icons-dsilver))
;; 	    (EnumMember . ,(my-company-box-icon 'material "people" 'all-the-icons-lcyan))
;; 	    (Constant . ,(my-company-box-icon 'material "pause_circle_filled" 'all-the-icons-blue))
;; 	    (Struct . ,(my-company-box-icon 'material "streetview" 'all-the-icons-red))
;; 	    (Event . ,(my-company-box-icon 'material "event" 'all-the-icons-red))
;; 	    (Operator . ,(my-company-box-icon 'material "control_point" 'all-the-icons-red))
;; 	    (TypeParameter . ,(my-company-box-icon 'material "class" 'all-the-icons-red))
;; 	    (Template   . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	    (Yasnippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	    (ElispFunction . ,(my-company-box-icon 'material "functions" 'all-the-icons-red))
;; 	    (ElispVariable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	    (ElispFeature . ,(my-company-box-icon 'material "stars" 'all-the-icons-orange))
;; 	    (ElispFace . ,(my-company-box-icon 'material "format_paint" 'all-the-icons-pink)))))
;;   )

(use-package company-flx
  :straight t
  :after company
  :custom
  (company-flx-limit 256)
  :config
  (company-flx-mode 1))

(use-package company-prescient
  :straight t
  :config (company-prescient-mode 1))

(use-package company-quickhelp
  :straight t
  :after company
  :config
  (company-quickhelp-mode))

;; ;; try nox
;; (use-package nox
;;   :straight (nox
;; 	     :host github
;; 	     :repo "manateelazycat/nox"
;; 	     :files ("nox.el"))

;;   :hook ((go-mode lua-mode c-mode-common c-mode c++-mode) . nox-ensure)
;;   :config
;;   ;; emmylua
;;   (let ((emmylua-jar-path (expand-file-name "bin/EmmyLua-LS-all.jar" poly-local-dir)))
;;     (add-to-list 'nox-server-programs
;; 		 `(lua-mode  . ("/usr/bin/java" "-cp" ,emmylua-jar-path "com.tang.vscode.MainKt"))))

;;   (setq nox-workspace-configuration
;;         '((:gopls . (:usePlaceholders t
;; 				      :completeUnimported  t
;; 				      :experimentalWorkspaceModule t))))
;;   (when (executable-find "gopls")
;;     (add-to-list 'nox-server-programs `(go-mode . ("gopls" "-logfile=/tmp/gopls.log" "-rpc.trace" "-vv" "--debug=localhost:6060"))))

;;   (when (executable-find "ccls")
;;     (add-to-list 'nox-server-programs '((c-mode c++-mode) "ccls"
;;  					"-init={\"compilationDatabaseDirectory\":\"build\"}")))

;;   (add-hook 'nox-managed-mode-hook (lambda()
;; 				     (make-local-variable 'company-backends)
;; 				     (setq company-backends nil)
;; 				     (setq company-backends
;; 					   '((company-tabnine :with company-capf :separate)
;; 					     company-dabbrev-code
;; 					     (company-files          ; files & directory
;; 					      company-keywords       ; keywords
;; 					      )
;; 					     (company-abbrev company-dabbrev)))))
;;   )

;;     (dolist (hook (list
;; 		   'js-mode-hook
;; 		   'rust-mode-hook
;; 		   'python-mode-hook
;; 		   'ruby-mode-hook
;; 		   'java-mode-hook
;; 		   'sh-mode-hook
;; 		   'php-mode-hook
;; 		   'c-mode-common-hook
;; 		   'c-mode-hook
;; 		   'c++-mode-hook
;; 		   'haskell-mode-hook
;; 		   'lua-mode-hook
;; 		   ))
;;       (add-hook hook '(lambda () (nox-ensure))))
;;     )

(provide 'init-company)
;;; init-company.el ends here
