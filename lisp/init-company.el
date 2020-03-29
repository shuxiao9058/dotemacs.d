;;; lisp/init-company.el -*- lexical-binding: t; -*-

;; ;; https://github.com/makp/emacs-configs/blob/831d07582269fb63caec087fe71bee398a8b2af8/mk_company.el
;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas)
;;           (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; Integrate company-tabnine with lsp-mode
(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
	  (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
	  candidates-capf
	  candidates-tabnine)
      (dolist (candidate candidates)
	(if (eq (get-text-property 0 'company-backend candidate)
		'company-tabnine)
	    (unless (gethash candidate candidates-table)
	      (push candidate candidates-tabnine))
	  (push candidate candidates-capf)
	  (puthash candidate t candidates-table)))
      (setq candidates-capf (nreverse candidates-capf))
      (setq candidates-tabnine (nreverse candidates-tabnine))
      (nconc (seq-take candidates-tabnine company-tabnine-max-num-results)
	     (seq-take candidates-capf (- 9 company-tabnine-max-num-results))))))

(use-package company
    :straight t
    :ensure t
    :custom
    (company-minimum-prefix-length 2)
    (company-tooltip-align-annotations t)
    (company-begin-commands '(self-insert-command))
    ;; (company-require-match nil)
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode eaf-mode))
    ;; Trigger completion immediately.
    (company-idle-delay 0.1)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (company-show-numbers t)
    :hook (after-init . global-company-mode)
    :config
    ;; set default `company-backends'
    ;; completion-at-point-functions
    (setq-default company-backends
		  '((company-tabnine :with company-capf :separate)
		    company-dabbrev-code
		    (company-files          ; files & directory
		     company-keywords       ; keywords
		     )
		    (company-abbrev company-dabbrev)
		    )
		  )
    ;; )
    :general
    (:keymaps '(company-active-map)
	      "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
	      "C-n"     #'company-select-next
	      "C-p"     #'company-select-previous
	      "C-j"     #'company-select-next
	      "C-k"     #'company-select-previous
	      "C-h"     #'company-show-doc-buffer
	      "C-u"     #'company-previous-page
	      "C-d"     #'company-next-page
	      "C-s"     #'company-filter-candidates
	      "C-S-s"  #'counsel-company
	      "C-SPC"   #'company-complete-common
	      "TAB"     #'company-complete-common-or-cycle
	      [tab]     #'company-complete-common-or-cycle
	      [backtab] #'company-select-previous
	      [f1]      nil
	      )
    (:keymaps '(company-search-map)
	      "C-n"     #'company-select-next-or-abort
	      "C-p"     #'company-select-previous-or-abort
	      "C-j"     #'company-select-next-or-abort
	      "C-k"     #'company-select-previous-or-abort
	      "C-s"     (lambda () (interactive) (company-search-abort) (company-filter-candidates))
	      [escape]  #'company-search-abort)
    )

;; (use-package company-posframe
;;     :straight t
;;     :after company
;;     :if IS-GUI
;;     ;; :if  (and (not (package-installed-p 'company-box))
;;     ;; 	      (>= emacs-major-version 26)
;;     ;; 	      IS-GUI)
;;     :hook (global-company-mode . company-posframe-mode)
;;     )

;; (use-package company-box
;;     :straight t
;;     ;; :straight (company-box :local-repo "~/workspace/company-box")
;;     :after company
;;     :hook (company-mode . company-box-mode)
;;     :if (and (>= emacs-major-version 26) IS-GUI)
;;     :diminish
;;     :custom
;;     (company-box-enable-icon IS-GUI)
;;     (company-box-doc-enable t)     ; eldoc performance issue
;;     (company-box-max-candidates 50)
;;     (company-box-show-single-candidate t)
;;     (company-box-doc-delay 0.3)
;;     :config
;;     (add-to-list 'company-box-backends-colors '(company-tabnine . "gold4"))
;;     (add-to-list 'company-box-backends-colors '(company-predictive . "darkred"))
;;     (add-to-list 'company-box-backends-colors '(company-dabbrev-code . "maroon"))
;;     (setq company-frontends (delq 'company-pseudo-tooltip-frontend company-frontends))

;;     (with-eval-after-load 'all-the-icons
;;       (defun +company-box-icons--elisp (candidate)
;; 	(when (derived-mode-p 'emacs-lisp-mode)
;; 	  (let ((sym (intern candidate)))
;; 	    (cond ((fboundp sym) 'ElispFunction)
;; 		  ((boundp sym) 'ElispVariable)
;; 		  ((featurep sym) 'ElispFeature)
;; 		  ((facep sym) 'ElispFace)))))

;;       (defun my-company-box-icon (family icon face &rest args)
;; 	"Defines icons using `all-the-icons' for `company-box'."
;; 	(when icon
;; 	  (pcase family
;; 	    ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 :face face args))
;; 	    ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575 :face face))
;; 	    ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 :face face args))
;; 	    ('alltheicon (all-the-icons-alltheicon icon :height 0.8 :face face args)))))

;;       (setq company-box-icons-alist 'company-box-icons-all-the-icons
;; 	    company-box-backends-colors nil
;; 	    company-box-icons-functions
;; 	    '(company-box-icons--yasnippet company-box-icons--lsp +company-box-icons--elisp company-box-icons--acphp)
;; 	    company-box-icons-all-the-icons
;; 	    `((Unknown . ,(my-company-box-icon 'material "find_in_page" 'all-the-icons-purple))
;; 	      (Text . ,(my-company-box-icon 'material "text_fields" 'all-the-icons-green))
;; 	      (Method . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	      (Function . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	      (Constructor . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
;; 	      (Field . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	      (Variable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	      (Class . ,(my-company-box-icon 'faicon "cog" 'all-the-icons-orange))
;; 	      (Interface . ,(my-company-box-icon 'faicon "info" 'all-the-icons-orange))
;; 	      (Module . ,(my-company-box-icon 'faicon "cogs" 'all-the-icons-orange))
;; 	      (Property . ,(my-company-box-icon 'material "settings" 'all-the-icons-dyellow))
;; 	      (Unit . ,(my-company-box-icon 'faicon "tag" 'all-the-icons-orange))
;; 	      (Value . ,(my-company-box-icon 'material "filter_none" 'all-the-icons-blue))
;; 	      (Enum . ,(my-company-box-icon 'faicon "list-ul" 'all-the-icons-lcyan))
;; 	      (Keyword . ,(my-company-box-icon 'material "filter_center_focus" 'all-the-icons-red))
;; 	      (Snippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	      (Color . ,(my-company-box-icon 'material "color_lens" 'all-the-icons-pink))
;; 	      (File . ,(my-company-box-icon 'material "insert_drive_file" 'all-the-icons-dsilver))
;; 	      (Reference . ,(my-company-box-icon 'material "collections_bookmark" 'all-the-icons-red))
;; 	      (Folder . ,(my-company-box-icon 'material "folder_open" 'all-the-icons-dsilver))
;; 	      (EnumMember . ,(my-company-box-icon 'material "people" 'all-the-icons-lcyan))
;; 	      (Constant . ,(my-company-box-icon 'material "pause_circle_filled" 'all-the-icons-blue))
;; 	      (Struct . ,(my-company-box-icon 'material "streetview" 'all-the-icons-red))
;; 	      (Event . ,(my-company-box-icon 'material "event" 'all-the-icons-red))
;; 	      (Operator . ,(my-company-box-icon 'material "control_point" 'all-the-icons-red))
;; 	      (TypeParameter . ,(my-company-box-icon 'material "class" 'all-the-icons-red))
;; 	      (Template   . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	      (Yasnippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
;; 	      (ElispFunction . ,(my-company-box-icon 'material "functions" 'all-the-icons-red))
;; 	      (ElispVariable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
;; 	      (ElispFeature . ,(my-company-box-icon 'material "stars" 'all-the-icons-orange))
;; 	      (ElispFace . ,(my-company-box-icon 'material "format_paint" 'all-the-icons-pink)))))
;;     )

(use-package company-flx
    :straight t
    :after company
    :config
    (company-flx-mode 1)
    (setq company-flx-limit 256))

;; (use-package company-prescient
;;     :straight t
;;     :disabled
;;     :ensure t
;;     :after company
;;     :config (company-prescient-mode))

;; (use-package company-quickhelp
;;     :straight t
;;     :after company
;;     :config
;;     (company-quickhelp-mode))

(use-package company-tabnine
    :straight t
    :commands company-tabnine-start-process
    :ensure t
    :after company
    :custom
    ;; (company-tabnine-max-num-results 9)
    (company-tabnine-no-continue t)
    :init
    (setq company-tabnine-executable-args
	  '("--client" "emacs" "--log-level" "Error" "--log-file-path" "/tmp/TabNine.log"))
    :config
    (setq company-tabnine-max-num-results 9)
    (when (> 9 company-tabnine-max-num-results)
      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
      )
    ;; workaround for company-flx-mode and other transformers
    (setq company-tabnine--disable-next-transform nil)
    (defun my-company--transform-candidates (func &rest args)
      (if (not company-tabnine--disable-next-transform)
          (apply func args)
	(setq company-tabnine--disable-next-transform nil)
	(car args)))

    (defun my-company-tabnine (func &rest args)
      (when (eq (car args) 'candidates)
	(setq company-tabnine--disable-next-transform t))
      (apply func args))

    (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
    (advice-add #'company-tabnine :around #'my-company-tabnine)
    )

;; ;; try nox
;; (use-package nox
;;     :straight (nox
;; 	       :host github
;; 	       :repo "manateelazycat/nox"
;; 	       :files ("nox.el"))
;;     ;; :after company
;;     :config
;;     ;; emmylua
;;     (let ((emmylua-jar-path (expand-file-name "bin/EmmyLua-LS-all.jar" poly-local-dir)))
;;       (add-to-list 'nox-server-programs
;; 		   `(lua-mode  . ("/usr/bin/java" "-cp" ,emmylua-jar-path "com.tang.vscode.MainKt")))
;;       )

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

(use-package yasnippet
    :straight t
    :ensure t
    :diminish yas-global-mode
    :commands yas-global-mode
    :hook (after-init . yas-global-mode)
    :config
    ;; (add-to-list 'yas-snippet-dirs
    ;; 		 (expand-file-name "snippets" poly-etc-dir))
    ;; make company break completion
    (setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet))
    )

(use-package yasnippet-snippets
    :straight t
    :ensure t
    :after yasnippet)

(provide 'init-company)
;;; init-company.el ends here
