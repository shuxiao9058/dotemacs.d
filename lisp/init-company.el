;;; lisp/init-company.el -*- lexical-binding: t; -*-

(use-package company
    :straight t
    :ensure t
    :custom
    (company-minimum-prefix-length 1)
    (company-tooltip-align-annotations t)
    (company-begin-commands '(self-insert-command))
    (company-require-match 'never)
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode eaf-mode))
    ;; Trigger completion immediately.
    (company-idle-delay 0.2)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (company-show-numbers t)
    :hook (after-init . global-company-mode)
    :config
    ;; (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
    ;;	  company-dabbrev-other-buffers 'all)

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
    :config
    (setq-default company-backends
		  (let ((b #'company-tabnine))
		    (cons b (remove b company-backends))))

    ;; ;; Enable TabNine on default
    ;; (add-to-list 'company-backends #'company-tabnine)

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

    (add-hook 'lsp-after-open-hook
	      (lambda ()
		(setq company-tabnine-max-num-results 3)
		(add-to-list 'company-transformers 'company//sort-by-tabnine t)
		(add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
    )

;; (use-package yasnippet :ensure t
;;   :straight t
;;   :config
;;   (add-hook 'after-init-hook 'yas-global-mode)
;;   (add-to-list 'yas-snippet-dirs
;;                (expand-file-name "snippets" poly-etc-dir))
;;   (setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet)) ; make company break completion

(provide 'init-company)
;;; init-company.el ends here
