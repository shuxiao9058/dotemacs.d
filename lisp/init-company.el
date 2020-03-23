;;; lisp/init-company.el -*- lexical-binding: t; -*-

;; https://github.com/makp/emacs-configs/blob/831d07582269fb63caec087fe71bee398a8b2af8/mk_company.el
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;; Integrate company-tabnine with lsp-mode
(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
	  (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
	  candidates-lsp
	  candidates-tabnine)
      (dolist (candidate candidates)
	(if (eq (get-text-property 0 'company-backend candidate)
		'company-tabnine)
	    (unless (gethash candidate candidates-table)
	      (push candidate candidates-tabnine))
	  (push candidate candidates-lsp)
	  (puthash candidate t candidates-table)))
      (setq candidates-lsp (nreverse candidates-lsp))
      (setq candidates-tabnine (nreverse candidates-tabnine))
      (nconc (seq-take candidates-tabnine 3)
	     (seq-take candidates-lsp 6)))))

(use-package company
    :straight t
    :ensure t
    :custom
    (company-minimum-prefix-length 3)
    (company-tooltip-align-annotations t)
    (company-begin-commands '(self-insert-command))
    (company-require-match 'never)
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode eaf-mode))
    ;; Trigger completion immediately.
    (company-idle-delay 0.5)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (company-show-numbers t)
    :hook (after-init . global-company-mode)
    :config
    ;; set default `company-backends'
    (setq-default company-backends
		  '((company-capf  ;; completion-at-point-functions
		     company-dabbrev-code)
		    (company-files          ; files & directory
		     company-keywords       ; keywords
		     )
		    (company-abbrev company-dabbrev)
		    ))
    )

(use-package company-posframe
    :straight t
    :after company
    :config
    (company-posframe-mode 1)
    )

(use-package company-flx
    :straight t
    :after company
    :config
    (company-flx-mode 1)
    (setq company-flx-limit 256))

(use-package company-prescient
    :straight t
    :disabled
    :ensure t
    :after company
    :config (company-prescient-mode))

(use-package company-quickhelp
    :straight t
    :after company
    :config
    (company-quickhelp-mode))

(use-package company-tabnine
    :straight t
    :commands company-tabnine-start-process
    :ensure t
    :after company
    :custom
    (company-tabnine-max-num-results 9)
    :init
    (setq company-tabnine-no-continue nil)
    ;; (setq company-tabnine-log-file-path "/tmp/TabNine.log")
    (setq company-tabnine-executable-args
          '("--client" "emacs" "--log-level" "Debug" "--log-file-path" "/tmp/TabNine.log"))
    :hook (lsp-after-open . (lambda ()
			      (setq company-tabnine-max-num-results 3)
			      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
			      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))
			      ;; Support yas in commpany
			      ;; Note: Must be the last to involve all backends
			      (setq-local company-backends
					  (mapcar #'company-mode/backend-with-yas company-backends))
			      )
			  )
    )

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
